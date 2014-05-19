package selfExecutingCmdLine;

//author: Aritra Dhar
//Intern
//Accenture Technology Labs
//version 1.4

import java.io.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;

public class caller_analyzer_Actor_DIR 
{
	public static void main(String[] args) throws Exception
	{
		//long startTime = System.currentTimeMillis();
		String filename=new String("");
		String rulepath=new String("");
		String querypath=new String("");
		String dirname=new String("");
		Integer selection=0;
		
		/*
		JavaAnalysis jn=new JavaAnalysis();
		String inputDir="C:\\test\\taintsource";
		String outputDir="C:\\test\\out";
		String libDir="C:\\test\\lib";
		
		jn.doAnalysis(inputDir, outputDir, libDir);
		System.out.println("Soot operation complete");
		*/
		
		if(args.length==0)
		{
			System.out.println("Error!! no argument found");
			System.exit(1);
		}
		
		if(args.length==1 && args[0].equals("-help"))
		{
			System.out.println("enter in format : java -jar \"analyzer_builder<ver_no>.jar -xml <XMLfile path> or -xml-dir <xmlfile dir> -rule <datalog rule file path> -query <datalog query file path>\"");
			System.exit(0);
		}
		
		if(args.length==6)
		{
			if(args[0].compareTo("-xml")==0 && args[2].compareTo("-rule")==0 && args[4].compareTo("-query")==0)
			{
				filename=args[1];
				rulepath=args[3];
				querypath=args[5];
				selection=1;
				//System.out.println("a");
			}
			
			if(args[0].compareTo("-xml-dir")==0 && args[2].compareTo("-rule")==0 && args[4].compareTo("-query")==0)
			{
				dirname=args[1];
				rulepath=args[3];
				querypath=args[5];
				selection=2;
				//System.out.println("b");
			}
		}
		
		else
		{
			System.out.println("Wrong no of arguments, enter -help option to see help");
		}
					    
		
		BufferedReader br2=null;
		
		FileWriter rwrite=new FileWriter("rule_path.txt");
		rwrite.append(rulepath);
		rwrite.close();
	
		FileWriter qwrite=new FileWriter("query_path.txt");
		qwrite.append(querypath);		
		qwrite.close();
		
	    br2=new BufferedReader(new FileReader(querypath));
		String qry="",r="";
		while((r=br2.readLine())!=null)
		{
			qry=r;
		}
		br2.close();
		
		
		
		File dir_name=null;
		File []filename_arr = null;	
		
		if(selection==2)
		{
			dir_name=new File(dirname);
			filename_arr=dir_name.listFiles();
			new DirClear().clear(dirname);
			long timeout=350;
			System.out.println("------ old cache cleared waiting for "+timeout+" ms. timeout ------");
			try
			{
				Thread.sleep(timeout);
			}
			catch(Exception e)
			{
				System.out.println(e.getMessage());
			}
		}
		
		if(selection==1)
		{
			filename_arr=new File[1];
			filename_arr[0]=new File(filename);
		}
		
		
		FileWriter []fwrite=new FileWriter[filename_arr.length];
		
		for(int k=0;k<filename_arr.length;k++)
		{
			System.out.println(filename_arr[k].toString());
		}
		
		FileWriter fwrite_check=new FileWriter("checkrun.log",true);
		DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
		Calendar cal = Calendar.getInstance();
		fwrite_check.append("Run "+dateFormat.format(cal.getTime())+"\n");
		fwrite_check.close();
		
		for(int k=0;k<filename_arr.length;k++)
		{
			String tmp=filename_arr[k].toString();
			if(tmp.contains(".xml"))
				filename=filename_arr[k].toString();
			else
				continue;
			
			System.out.println("==== "+filename+" ====");
			
			fwrite[k]=new FileWriter("path.txt");
			fwrite[k].append(filename);
			fwrite[k].close();
					
			System.out.println("---- Removing DOC type tag from the XML ----");
			DoctypeRemover dr=new DoctypeRemover();			
			dr.setFilePath(filename);
			dr.DocTypeRemover();			
			
			System.out.println("----- Intermediate xml file generator ----");
			XMLParser_builder xp=new XMLParser_builder();
			xp.main(null);
			System.out.println("----- done ----");

			System.out.println("\n----- Datalog file <with alternative facts> generator ----");
			DatalogRulrGenV2 dg=new DatalogRulrGenV2();
			dg.main(null);
		}	
		
		//System.gc();
	    
		File dl_dir_name=null;
		File []dl_file_name_arr=null;
	
		if(selection==2)
		{
			dl_dir_name=new File(dirname.concat("\\datalog\\"));
			dl_file_name_arr=dl_dir_name.listFiles();
		}
		if(selection==1)
		{
			FileNameGen fg=new FileNameGen();
			fg.filenameGenerator(filename);
			dl_file_name_arr=new File[1];
			//System.out.println(fg.loc().concat("datalog\\").concat(fg.filename_s()).concat(".dl"));
			dl_file_name_arr[0]=new File(fg.loc().concat("datalog\\").concat(fg.filename_s()).concat("_datalog.dl"));
		}
		
		
		Integer thread_count=0;
		FileWriter fw=new FileWriter("Actorlog.log");		
		DatalogPerser dp_temp=null;
		for(int k=0;k<dl_file_name_arr.length;k++)
		{
			dp_temp=new DatalogPerser();
			dp_temp.method_detector(dl_file_name_arr[k].toString());
			thread_count+=dp_temp.method_count();			
		}
		dp_temp=null;	//make it eligible for garbage collection	
		fw.append("Total actor count = "+thread_count.toString());
		fw.close();
		
		System.out.println("Free heap : "+Runtime.getRuntime().freeMemory());
		System.gc();
		System.out.println("Free heap after gc() : "+Runtime.getRuntime().freeMemory());
		
		for(int k=0;k<dl_file_name_arr.length;k++)
		{			
									
			DatalogPerser dp=new DatalogPerser();
	 		String current_file_path=dl_file_name_arr[k].toString();
			String []s_cp=dp.method_detector(current_file_path);
			
			System.out.println("\n----<actor submit> Executing datalog facts for :"+current_file_path+"----\n");	
			
			Runner rn=new Runner();
			String []ar=new String[3];
			ar[1]=qry;
			ar[2]=current_file_path.substring(0, current_file_path.indexOf(".dl")).concat("_output.txt");
			for(int i=1;i<dp.method_count();i++)
			{
				ar[0]=s_cp[i];
				
				rn.main(ar);
				//System.out.println("STARTED FOR"+ar[0]);
				//System.out.println("^^^^^^^^^^^^^^^^^^^^^^^^^^^");
			}
		 }
		//System.out.println("file count"+ dl_file_name_arr.length);
		
		//long endTime   = System.currentTimeMillis();
		//long totalTime = endTime - startTime;
		//System.out.println("---- Total Program Execution time : "+totalTime+"ms. ----");
		
		
		
	}

}
