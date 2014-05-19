package selfExecutingCmdLine;

import java.io.*;

public class DirClear 
{
    public static boolean deleteDir(File dir) 
    {
        if (dir.isDirectory()) 
        {
                String[] children = dir.list();
                for (int i = 0; i < children.length; i++) 
                {
                        boolean success = deleteDir(new File(dir, children[i]));
                        if (!success) 
                           return false;
                }
        }
        return dir.delete();
    }

	public void clear(String path)
	{
		File filepath=new File(path);
		if(!filepath.exists() && !filepath.isDirectory())
		{
			System.out.println("Folder not exists.. exiting");
			System.exit(1);
		}
		File []filearr=filepath.listFiles();
		Integer dircount=0,j=0;
		for(int i=0;i<filearr.length;i++)
		{
			if(!filearr[i].toString().contains(".xml"))
			{
				dircount++;
			}
		}
		
		String []tobedeleted=new String[dircount];
		
		for(int i=0;i<filearr.length;i++)
		{
			if(!filearr[i].toString().contains(".xml"))
			{
				tobedeleted[j]=filearr[i].toString();
				j++;
			}
		}
		
		filearr=null;
		System.gc();
		
		for(int i=0;i<dircount;i++)
		{		
			deleteDir(new File(tobedeleted[i]));
		}
	}
}
