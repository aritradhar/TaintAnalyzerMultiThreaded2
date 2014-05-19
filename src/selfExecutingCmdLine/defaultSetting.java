package selfExecutingCmdLine;


public class defaultSetting 
{
	public static void main(String []arg) throws Exception
	{
		if(arg.length==1 && arg[0].equalsIgnoreCase("-help"))
		{
			System.out.println("java -jar <jar_name> -<actor/thread/tpool> " +
					"<input_dir> <output_dir> <lib_dir> <rule_file> <query_file>" +
					"\n actor - execution in scala actor" +
					"\n thread - execution in java thread" +
					"\n tpool - execution in java thread pool ");
			System.exit(0);
		}
		if(arg.length<6 || arg.length>6)
		{
			System.out.println("Wrong no. of arguments, enter -help option for help");
			System.exit(0);
		}
		if(arg.length==6 && (arg[0].equalsIgnoreCase("-actor")||
				             arg[0].equalsIgnoreCase("-thread")||
				             arg[0].equalsIgnoreCase("-tpool")))
		{
			JavaAnalysis jn=new JavaAnalysis();
			String inputDir=arg[1];
			String outputDir=arg[2];
			String libDir=arg[3];			
			jn.doAnalysis(inputDir, outputDir, libDir);
			
			String []ag=new String[6];
			ag[0]="-xml-dir";
			ag[1]=outputDir;
			ag[2]="-rule";
			ag[3]=arg[4];
			ag[4]="-query";
			ag[5]=arg[5];
			
			if(arg[0].equalsIgnoreCase("-actor"))
				caller_analyzer_Actor_DIR.main(ag);
			if(arg[0].equalsIgnoreCase("-thread"))
				caller_analyzer_MT_DIR.main(ag);
			if(arg[0].equalsIgnoreCase("-tpool"))
				caller_analyzer_ThreadPool_DIR.main(ag);
		}
	}
}
