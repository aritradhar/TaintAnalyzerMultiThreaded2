package selfExecutingCmdLine;

//author: Aritra Dhar
//Intern
//Accenture Technology Labs
//version 1.4

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;

class Datalog_executor implements Runnable {

	String fact_str,qry_str;
	public Datalog_executor(String fact,String qry) 
	{
		this.fact_str=fact;
		this.qry_str=qry;
	}
    public void run()
    {
    	Datalog_query dl=new Datalog_query();
    	dl.loadRules(fact_str);
    	dl.query(qry_str,null);
    }
}

public class caller_analyzer_MultiThread 
{
	public static void main(String[] args) throws Exception
	{
		long startTime = System.currentTimeMillis();
		String filename=new String("");
		String rulepath=new String("");
		String querypath=new String("");
		
		if(args.length==0)
		{
			System.out.println("Error!! no argument found");
			System.exit(1);
		}
		
		if(args.length==1 && args[0].equals("-help"))
		{
			System.out.println("enter in format : java -jar \"analyzer_builder<ver_no>.jar -xml <XMLfile path> -rule <datalog rule file path> -query <datalog query file path>\"");
			System.exit(0);
		}
		
		if(args.length==6)
		{
			if(args[0].equals("-xml") && args[2].equals("-rule") && args[4].equals("-query"))
			{
				filename=args[1];
				rulepath=args[3];
				querypath=args[5];
			}
		}
		else
		{
			System.out.println("Wrong no of arguments, enter -help option to see help");
		}
	
				
		
		System.out.println("---- Removing DOC type tag from the XML ----");
		DoctypeRemover dr=new DoctypeRemover();			
		dr.setFilePath(filename);
		dr.DocTypeRemover();
		
		FileWriter fwrite=new FileWriter("path.txt");
		fwrite.append(filename);
		fwrite.close();
		
		FileWriter rwrite=new FileWriter("rule_path.txt");
		rwrite.append(rulepath);
		rwrite.close();
		
		FileWriter qwrite=new FileWriter("query_path.txt");
		qwrite.append(querypath);		
		qwrite.close();
		
		System.out.println("----- Intermediate xml file generator ----");
		XMLParser_builder xp=new XMLParser_builder();
		xp.main(null);
		
		System.out.println("----- done ----");

		System.out.println("\n----- Datalog file <with alternative facts> generator ----");
		DatalogRulrGenV2 dg=new DatalogRulrGenV2();
		dg.main(null);
		
		
		System.out.println("\n----Executing datalog facts ----\n");
		
		
		BufferedReader br1=new BufferedReader(new FileReader("fact_path.txt"));
		String fact_path="",r="";
		while((r=br1.readLine())!=null)
		{
			fact_path=r;
		}
		br1.close();
		
		BufferedReader br2=new BufferedReader(new FileReader(querypath));
	    String qry="";r="";
		while((r=br2.readLine())!=null)
		{
			qry=r;
		}
		br2.close();
		
		DatalogPerser dp=new DatalogPerser();
				
		String []s_cp=dp.method_detector(fact_path);


		Thread []T=new Thread[dp.method_count()];
		
		for(int i=0;i<dp.method_count();i++)
			T[i]=new Thread(new Datalog_executor(s_cp[i],qry));
		for(int i=0;i<dp.method_count();i++)
			T[i].start();
	
		
		
		long endTime   = System.currentTimeMillis();
		long totalTime = endTime - startTime;
		System.out.println("---- Total Program Execution time : "+totalTime+"ms. ----");
		
		
		
	}

}
