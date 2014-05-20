package selfExecutingCmdLine

class FileNameGen 
{
  
  var loc:String=""	   
  var filename_s:String=""
  var dlmtr:String="_"
    
  def filenameGenerator(s:String):Unit =
  {
    var len:Int=s.length()
    var loc_pos:Int=0
    var i:Int=0
    if(s.contains("\\"))
    {
    	for(i <-0 to len-1)
    	{
    		if(s.charAt(i)=='\\')
    		{
    			loc_pos=i
    		}
    	}
    	loc=s.substring(0,loc_pos+1)
    	var fullfilename:String=s.substring(loc_pos+1,s.length())
    	//println("full filename="+fullfilename)
    	filename_s=fullfilename.substring(0, fullfilename.indexOf(".xml"))
    	//println("filename="+filename_s)
    	
    }
    else
    {
      loc=""
      filename_s=s.substring(0, s.indexOf(".xml"))
     // println("filename="+filename_s)
    }
  }
}