package selfExecutingCmdLine

//author: Aritra Dhar
//Intern
//Accenture Technlology Labs
//version 2.4

import java.io._
import java.io.InputStreamReader
import scala.xml.XML
import javax.xml.soap.Text
import javax.xml.soap.Text
import java.util._
import java.util.Iterator
import scala.Mutable
import scala.collection.JavaConversions._
import scala.collection.immutable.Set
import java.lang.Integer
import scala.io.Source



class XMLParser_builder
{

  def main(ar:Array[String])=
  {
  var loc:String=""	   
  var tempFile:String = ""
  val temp:String=""
  for(temp <- Source.fromFile("path.txt").getLines())
  ////println(temp)
  tempFile=temp
  //println("tempfile="+tempFile)
  var filename_actual:String=""
  var filename_s:String=""
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
      //println("filename="+filename_s)
    }
    ////print(loc_pos)
  }
  filenameGenerator(tempFile)
  //println("loc="+loc)
  
  var filename= Set(filename_s)
  //
  def delemeterString(S:String):Array[String] =
  {
    var data:Array[String]=S.split("\",\"")
    return data
  }
  
  def arg_counter(arg:String):Int =
  {
    if(arg.length()==0)
      return 0;
    
    var arg_len:Int=arg.length()
    var angular_brac_count:Int=0
    var arg_count=0
    
    var i:Int=0
    for(i <-0 to arg_len-1)
    {
      if(arg.charAt(i)=='<')
        angular_brac_count=angular_brac_count+1
       if(arg.charAt(i)=='>')
         angular_brac_count=angular_brac_count-1
       if(angular_brac_count==0)
       {
         if(arg.charAt(i)==',')
           arg_count=arg_count+1
       }
    }
    return (arg_count+1);   
  }
  
  def loadXML(fileName:String) = 
    using(new FileInputStream(fileName))
    {
	  fis => using(new InputStreamReader(fis))
	  {
	    isr => scala.xml.XML.load(isr)
	  }
	}
  
  def encounter_equal(str:String,op:String) : Boolean =
  {
    if(str.contains(op))
    {
      ////print(str.indexOf(op))
      var pos:Int=0
      var i:Int=0
      for(i <-0 to str.length()-1)
      {
        if(str.charAt(i)=='=')
          pos=i
      }
      
      var st:String=str.substring(0,pos-1)
      if(st.contains(" "))
      {
        return false
      }
       else
          return true
    }
    else
      return false
  }
  
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B = 
	try 
  	{ 
	  f(param) 
	} 
  	finally 
  	{ 
  	  param.close() 
  	}
  	  //def formatNicely(xml:Elem) = new Pretty//printer(200, 2).format(xml)
  	
  //def appendToFile(fileName:String, textData:String) =using (new FileWriter(fileName,true)){ fileWriter => using (new //printWriter(fileWriter)) {
     // //printWriter => //printWriter.//println(textData)}}
  
	
	val extension=".xml"
	val out_extension:String="_gen.xml"
	//val out:String=filename.concat("_scala_summary.txt")
	filename.foreach(filename=>{
	 //print("\n **** persing file :"+filename+"**** \n")
	val XMLfile=loadXML(loc+filename+extension)
	val fname_scalaSummary:java.io.File=new File(loc+"ScalaSummary\\")
	if(!fname_scalaSummary.exists())
	 fname_scalaSummary.mkdir();
	val fname_xmlgen:java.io.File=new File(loc+"XMLgen\\")
	if(!fname_xmlgen.exists())
	 fname_xmlgen.mkdir();
    val fwrite:java.io.FileWriter=new FileWriter(loc+"ScalaSummary\\"+filename+"_scala_summary.txt")
    val XMLwriter:java.io.FileWriter=new FileWriter(loc+"XMLgen\\"+filename+out_extension)
	
	var jimple_stmt:String=new String("")
    // XMLfile.foreach ( method => {val locals = method \\ "local" 
    //  locals.foreach (l => //println (" method "+(method \ "@name" text)+" local "+(l \ "@name" text)))
    // }
    //)
	var interfacelist=XMLfile \\ "interfaces"
	//var interfacecount:Int=interfacelist.length
	////println("Total no of interfaces : "+interfacelist \ "@count")
	//fwrite.append("Total no of interfaces : "+interfacelist \ "@count"+"\n")
	//if(interfacecount!=0)
	//{  
	//}
	
    var classlist=XMLfile \\ "class"
    var classcount:Int=classlist.length
    var i:Int=0
    XMLwriter.append("<cfg_generator version = \"2.5\" author =\"Aritra Dhar\">\n")
    XMLwriter.append("	<class_i")
    //println("Total no of classes : "+classcount);fwrite.append("Total no of classes : "+classcount+"\n")
	
    classlist.foreach ( classlist => 
	{
	  //println("-------------------------------------------------------------------------------------------------------------------")
	  fwrite.append("-------------------------------------------------------------------------------------------------------------------\n");
	  
	  val method = classlist \\ "method"
	  
	  //println("classname : "+classlist \ "@name")
	  
	  fwrite.append("classname : "+classlist \ "@name"+"\n")
	  
	  XMLwriter.append(" name = \""+classlist \ "@name"+"\"")
	  
	  //println("Extends :"+ classlist \ "@extends")
	 
	  fwrite.append("Extends :"+ classlist \ "@extends"+"\n") 
	  
	  XMLwriter.append(" extends = \""+ classlist \ "@extends"+"\"")
	  
	  val modifierlist=classlist \\ "modifier"
	  
	  XMLwriter.append(" modifier = \""+modifierlist \ "@name"+"\">\n")
	  
	  //println("Access modifier: "+modifierlist \ "@name")
	  
	  fwrite.append("Access modifier: "+modifierlist \ "@name"+"\n")
	 
	  var fieldlist=classlist \\ "field"
	  
	  XMLwriter.append("		<fields>\n")
	  fieldlist.foreach(fld =>
	    {
	      //print("Field name = "+fld \ "@name"+", Field type = "+fld \ "@type")
	      fwrite.append("Field name = "+fld \ "@name"+", Field type = "+fld \ "@type")
	      XMLwriter.append("			<field name = \""+fld \ "@name"+"\" type = \""+fld \ "@type"+"\"/>\n")
	    }
	  )
	  XMLwriter.append("		</fields>\n")
	  method.foreach (l => 
	  {
	    //println("---------")
	    fwrite.append("---------\n")
	    //println ("Method name "+(l \ "@name" text))
	    fwrite.append("Method name "+(l \ "@name" text));fwrite.append("\n")
	    
	    //println("Return type :"+ l \ "@returntype")
	    fwrite.append("Return type :"+ l \ "@returntype"+"\n")
	    XMLwriter.append("		<method name = \""+l \ "@name"+"\" return_type = \""+l \ "@returntype"+"\"")
	    
	    val parameterlist= l \\ "parameter"
	    val parameter_count:Int=parameterlist.length
	    
	    //println("Parameter count :"+ parameter_count)
	    fwrite.append("Parameter count :"+ parameter_count+"\n")
	    XMLwriter.append(" parameter_count = \""+parameter_count+"\">\n")
	    
	    if(parameterlist.length==0)
	    {
	    	//println("parameter type: void")
	    	fwrite.append("parameter type: void\n")
	    	XMLwriter.append("			<parameter type = \" void \"/>\n")
	    }
	    else
	    {
	    	parameterlist.foreach(p =>
	    	{
	    	  //println("parameter type:"+p \ "@type")
	    	  fwrite.append("parameter type:"+p \ "@type"+"\n")
	    	  XMLwriter.append("			<parameter type = \""+p \ "@type"+"\"/>\n")
	    	})
	    }
	    //trace local
	    val locallist= l \\ "local"
	    //println("----List of local variables and type----")
	    fwrite.append("----List of local variables and type----\n")
	    
	    XMLwriter.append("			<local_var>\n")
	    var localmap: java.util.Map[String, String] = new java.util.HashMap()
	    var local_defmap: java.util.Map[String, String] = new java.util.HashMap()
	    //var locmap=new ScalHashMap[String,String]
	    
	    locallist.foreach(local => 
	    {
	    	  var def_stmt_line:String=local \\ "definition" \ "@line" text;
	    	  localmap.put(local \ "@name" text, local \ "@type" text)
	    	  local_defmap.put(local \ "@name" text, def_stmt_line)
	    	  //var key:String=local \ "@name" text
	    	  //var value:String=local \ "@type" text
	    	  //locmap+=(key -> value)
	    	  ////print("{"+local \ "@name"+"=" +local \ "@type"+"},")
	    	  //fwrite.append("{"+local \ "@name"+"=" +local \ "@type"+"},")
	    	 // XMLwriter.append("<local name = \""+local \ "@name"+"\""+" type = \""+local \ "@type"+"\"/>\n")
	      
	    })//local
	    //var key:String=""
	    //var value:String=""
	    
	    localmap.keySet().foreach(lm=>{XMLwriter.append("				<local name = \""+lm+"\""+" type = \""+localmap.get(lm)+"\" def = \""+local_defmap.get(lm)+"\"/>\n")})	 
	    //localmap.foreach(el =>{XMLwriter.append("				<local name = \""+el._1+"\""+" type = \""+el._2+"\"/>\n")})
	    //print(localmap)
	    fwrite.append(localmap.toString())
	    
	    //XMLwriter.append(localmap.toString())
	    localmap.keySet()
	    XMLwriter.append("			</local_var>\n")
	    //println()
	    fwrite.append("\n")
	    val statementlist=l \\"statement"
	    var i=0
	    //println("\n----start of jimple statement----")
	    fwrite.append("\n----start of jimple statement----\n")
	    XMLwriter.append("			<jimple_statement>\n")
	    
	    val labelmap: java.util.Map[String, String] = new java.util.HashMap()
	    
	    statementlist.foreach(stmt =>{
	        var lb:String=stmt \ "@label" text
	        var id:String=stmt \ "@id" text;
	        if(!labelmap.containsKey(lb))
	    	  labelmap.put(lb, id)	  
	    })
	    statementlist.foreach(stmt => 
	    { 	      
	      //print(stmt \ "@label"+" : ")
	      fwrite.append(stmt \ "@label"+" : ")
	      XMLwriter.append("				<jimple_stmt label_id = \""+stmt \ "@label"+"\" ")
	      
	      //print("<stmt id :"+stmt \ "@id"+"> ")
	      fwrite.append("<stmt id :"+stmt \ "@id"+"> ")
	      XMLwriter.append("stmt_id=\""+stmt \ "@id"+"\" ><![CDATA[")
	      
     	  var jimplelist=stmt \\ "jimple"  	  
	      jimplelist.foreach(jm=>
     	  {
     		  jimple_stmt=jm.text;	
     		  //print(jm text)
     		  fwrite.append(jm text)
     		  XMLwriter.append(jm text)
     	  })
     	  XMLwriter.append("]]>")
     	  
     	  
     	   //for goto statements trace statement id
     	   if(jimple_stmt.contains("goto label") && !jimple_stmt.contains("tableswitch") && !jimple_stmt.contains("lookupswitch"))
     	   {
     		   var goto_pos:Int=jimple_stmt.indexOf("goto")
     		   var label_start_pos:Int=goto_pos+5;
     		   var label_info:String=jimple_stmt.subSequence(label_start_pos,jimple_stmt.length()).toString()
     		   //print("  <goto stmt : "+labelmap.get(label_info)+">")
     		   fwrite.append("  <goto stmt : "+labelmap.get(label_info)+">")
     		   XMLwriter.append("\n					<statement type = \"gotostatement\"/>")
     		   XMLwriter.append("\n					<jump target = \"stmt_id : "+labelmap.get(label_info)+"\"/>")
     	   }
	      
	      //considering same for now
	      
	      //some places tableswitch is also var name, to ensure correctness check for {, case, :, ;
	      if((jimple_stmt.contains("tableswitch") && jimple_stmt.contains("{") && jimple_stmt.contains("case") 
	          && jimple_stmt.contains(":") && jimple_stmt.contains("goto") && jimple_stmt.contains(";")) 
	          || (jimple_stmt.contains("lookupswitch") && jimple_stmt.contains("{") && jimple_stmt.contains("case") 
	          && jimple_stmt.contains(":") && jimple_stmt.contains("goto") && jimple_stmt.contains(";")))
	      {
	        XMLwriter.append("\n					<statement type = \"switchstatement\"/>")
	        var open_brac:Int=jimple_stmt.indexOf("{")
	        var close_brac:Int=0
	        //vat i:Int=0
	        for(i<-0 to jimple_stmt.length()-1)
	        {
	          if(jimple_stmt.charAt(i)=='}')
	            close_brac=i
	        }
	        var case_stmt:String=jimple_stmt.substring(open_brac+1, close_brac).trim()
	        //print(case_stmt)
	        //count no of cases by ;
	        var case_count:Int=0
	        for(i <-0 to case_stmt.length()-1)
	        {
	          if(case_stmt.charAt(i)==';')
	            case_count=case_count+1
	        }
	        var case_arr:Array[String]=new Array(case_count)
	        case_arr=case_stmt.split(";")
	        
	        //extract level id and corresponding statement id from each case
	        case_arr.foreach(p=>{
	          var label_id_loc:String=p.substring(p.indexOf("goto")+5, p.length())
	          var stmt_id_loc:String=labelmap.get(label_id_loc)
	          XMLwriter.append("\n					<case_jump target= \"stmt_id : "+stmt_id_loc+"\"/>")
	          //println(stmt_id)
	        })
	      }
	      
	      //considering same ends
	      
	      
	      //for static invoke statement
	      if(jimple_stmt.contains("staticinvoke"))
	      {
	    	  //XMLwriter.append("\n<staticinvoke_stmt>")
	    	  
	    	  //print("\n          <")
	    	  
	    	  fwrite.append("\n          <")
	    	  XMLwriter.append("\n					<statement type =\"staticinvoke\"/>")
	    	    
	    	  var str:String="staticinvoke"
	          var len:Int=str.length()	          	     
	    	  if(encounter_equal(jimple_stmt,"="))
	    	  {
	    		  var equal_pos:Int=jimple_stmt.indexOf("=")
	    		  //print(equal_pos)
	    		  var lhs:String=jimple_stmt.substring(0, equal_pos-1)
	    		  
	    		  //print(" lhs = "+lhs+",")	    		  
	    		  fwrite.append(" lhs = "+lhs+",")	    		  	    		
	    		  XMLwriter.append("\n					<lhs name =\""+lhs+"\"/>")
	    		  
	    		  val start:Int=jimple_stmt.indexOf("<")
	    		  var i=0
	    		  var end:Int=0
	    		  
	    		  for(i<-0 to jimple_stmt.length()-1 )
	    		  {
	    			  if(jimple_stmt.charAt(i)=='>')
	    				  end=i
	    		  }
	    		  //val end:Int=jimple_stmt.indexOf(">")
	    		  val delmtr:Int=jimple_stmt.indexOf(":")
	    		  val class_name:String=jimple_stmt.substring(start+1, delmtr)
	    		  
	    		  //print(" class = "+class_name+",")
	    		  
	    		  fwrite.append(" class = "+class_name+",")
	    		  
	    		  XMLwriter.append("\n					<class name = \""+class_name+"\"/>")
	    		  
	    		  val full_func:String=jimple_stmt.substring(delmtr+2, end)
	    		  val ret_type:String=full_func.substring(0, full_func.indexOf(" "))
	    		  val fun_name:String=full_func.substring(full_func.indexOf(" ")+1, full_func.length())
	    		  var close_brac_pos:Int=0
	    		  
	    		  for(i<-0 to fun_name.length()-1)
	    		  {
	    			  if(fun_name.charAt(i)==')')
	    				  close_brac_pos=i
	    		  }
	    		  ////print("close brac pos"+close_brac_pos)
	    		  var func_name_only:String=fun_name.substring(0,fun_name.indexOf("(")+1)
	    		  
	    		  //print(" function name = "+func_name_only+"),")
	    		  //print(" return type = "+ret_type+",")
	    		  
	    		  fwrite.append(" function name = "+func_name_only+"),")
	    		  fwrite.append(" return type = "+ret_type+",")
	    		  
	    		    //< check start
	    		   if(func_name_only.contains("<")|| func_name_only.contains("<"))
	    		  {
	    		    XMLwriter.append("					<function name = \"")
	    		    for(i <-0 to func_name_only.length()-1)
	    		    {
	    		      if(func_name_only.charAt(i)=='<' || func_name_only.charAt(i)=='>')
	    		      {
	    		        //XMLwriter.append()
	    		      }
	    		      else
	    		        XMLwriter.append(func_name_only.charAt(i))
	    		        
	    		    }
	    		    XMLwriter.append(")\"/>")
	    		  }
	    		  else
	    		  {
	    			  XMLwriter.append("\n					<function name = \""+func_name_only+")\"/>")
	    		  }
	    		  XMLwriter.append("\n					<return type = \""+ret_type+"\"/>\n")
	    		  //< check end
	    		      
	    		  var parameter_list:String=fun_name.substring(fun_name.indexOf("(")+1,close_brac_pos)
	    		  
	    		  if(parameter_list.length()!=0)
	    		  {
	    			  //print(" parameter types = "+parameter_list+",")	    			  
	    			  fwrite.append(" parameter types = "+parameter_list+",")	    			  
	    			  XMLwriter.append("					<parameter_internal types = \""+parameter_list+"\"/>")
	    	 
	    			  var arg:String=jimple_stmt.substring(end+2, jimple_stmt.length()-1)
	    			  //arg="<![CDATA[".concat(arg).concat("]]")
	    			  
	    			  //print(" arg = "+arg+",")
	    			  
	    			  fwrite.append(" arg = "+arg+",")
	    			  
	    			  //arg processing start
	    			  if(arg.contains("\""))
	    			  {
	    				  if(arg.compareTo("\"\"")==0)
	    					  XMLwriter.append("\n					<arg><![CDATA[-empty string-]]></arg>")
	    				  else
	    				  {
	    				    XMLwriter.append("\n					<arg><![CDATA["+arg.replaceAll("\"", "")+"]]></arg>")
	    				  }
	    			  }
	    			  else
	    				XMLwriter.append("\n					<arg><![CDATA["+arg+"]]></arg>")
	    			//arg processing end
	    		  }
	    		  else
	    		  {
	    			  //print(" parameter types = void,")
	    			  fwrite.append(" parameter types = void,")
	    		  
	    			  //print(" arg = void,")
	    			  fwrite.append(" arg = void,")
	    			  
	    			  XMLwriter.append("					<parameter_internal types = \"void\"/>")
	    			  XMLwriter.append("\n					<arg><![CDATA[void]]></arg>")
	    		  }
	    	  }
	    	  else
	    	  {
	    	      //start
	    		  val start:Int=jimple_stmt.indexOf("<")
	    		  var i=0
	    		  var end:Int=0
	    		  
	    		  for(i<-0 to jimple_stmt.length()-1 )
	    		  {
	    			  if(jimple_stmt.charAt(i)=='>')
	    				  end=i
	    		  }
	    		  //st
	    		  val delmtr:Int=jimple_stmt.indexOf(":")
	    		  val class_name:String=jimple_stmt.substring(start+1, delmtr)
	    		  
	    		  //print(" class = "+class_name+",")	    		  
	    		  fwrite.append(" class = "+class_name+",")    		  
	    		  XMLwriter.append("\n					<class name = \""+class_name+"\"/>")
	    		  
	    		  val full_func:String=jimple_stmt.substring(delmtr+2, end)
	    		  val ret_type:String=full_func.substring(0, full_func.indexOf(" "))
	    		  val fun_name:String=full_func.substring(full_func.indexOf(" ")+1, full_func.length())
	    		  var close_brac_pos:Int=0
	    		  
	    		  for(i<-0 to fun_name.length()-1)
	    		  {
	    			  if(fun_name.charAt(i)==')')
	    				  close_brac_pos=i
	    		  }
	    		  ////print("close brac pos"+close_brac_pos)
	    		  var func_name_only:String=fun_name.substring(0,fun_name.indexOf("(")+1)
	    		  
	    		  //print(" function name = "+func_name_only+"),")
	    		  //print(" return type = "+ret_type+",")
	    		  
	    		  fwrite.append(" function name = "+func_name_only+"),")
	    		  fwrite.append(" return type = "+ret_type+",")
	    		  
	    		    //< check start
	    		   if(func_name_only.contains("<")|| func_name_only.contains("<"))
	    		  {
	    		    XMLwriter.append("					<function name = \"")
	    		    for(i <-0 to func_name_only.length()-1)
	    		    {
	    		      if(func_name_only.charAt(i)=='<' || func_name_only.charAt(i)=='>')
	    		      {
	    		        //XMLwriter.append()
	    		      }
	    		      else
	    		        XMLwriter.append(func_name_only.charAt(i))
	    		        
	    		    }
	    		    XMLwriter.append(")\"/>")
	    		  }
	    		  else
	    		  {
	    			  XMLwriter.append("\n					<function name = \""+func_name_only+")\"/>")
	    		  }
	    		  XMLwriter.append("\n					<return type = \""+ret_type+"\"/>\n")
	    		  //< check end
	    		  
	    		  var parameter_list:String=fun_name.substring(fun_name.indexOf("(")+1,close_brac_pos)
	    		  
	    		  if(parameter_list.length()!=0)
	    		  {
	    			  //print(" parameter types = "+parameter_list+",")	    			  
	    			  fwrite.append(" parameter types = "+parameter_list+",")	    			  
	    			  XMLwriter.append("\n					<parameter_internal types = \""+parameter_list+"\"/>")
	    	 
	    			  var arg:String=jimple_stmt.substring(end+2, jimple_stmt.length()-1)
	    			  //arg="<![CDATA[".concat(arg).concat("]]")
	    			  
	    			  //print(" arg = "+arg+",")	    			  
	    			  fwrite.append(" arg = "+arg+",")
	    			 
	    			  //arg processing start
	    			   if(arg.contains("\""))
	    			  {
	    				  if(arg.compareTo("\"\"")==0)
	    					  XMLwriter.append("\n					<arg><![CDATA[-empty string-]]></arg>")
	    				  else
	    				  {
	    				    XMLwriter.append("\n					<arg><![CDATA["+arg.replaceAll("\"", "")+"]]></arg>")
	    				  }
	    			  }
	    			  else
	    				XMLwriter.append("\n					<arg><![CDATA["+arg+"]]></arg>")
	    			//arg processing end
	    		  }
	    		  else
	    		  {
	    			  //print(" parameter types = void,")
	    			  fwrite.append(" parameter types = void,")
	    		  
	    			  //print(" arg = void,")
	    			  fwrite.append(" arg = void,")
	    			  
	    			  XMLwriter.append("					<parameter_internal types = \"void\"/>")
	    			  XMLwriter.append("\n					<arg><![CDATA[void]]></arg>")
	    		  }
	    	    
	    	    //end
	    	  }
	    	  ////print(start+":"+end)
	    	  //XMLwriter.append("\n</staticinvoke_stmt>")
	    	  
	    	  //print(" >")
	    	  
	    	  fwrite.append(" >")
	      }
	      
	      //for virtual invoke statement
	      if(jimple_stmt.contains("virtualinvoke"))
	      {
	    	  var str:String="virtualinvoke"
	    	  //print("\n          <")
	    	  fwrite.append("\n          <")
	    	  XMLwriter.append("\n					<statement type =\"virtualinvoke\"/>")
	    	  var len:Int=str.length()	
	    	  
	    	  if(encounter_equal(jimple_stmt,"="))
	    	  {	   
	    		  var equal_pos:Int=jimple_stmt.indexOf("=")
	    		  ////print(equal_pos)
	    		  var lhs:String=jimple_stmt.substring(0, equal_pos-1)
	    		  
	    		  //print(" lhs = "+lhs+",")
	    		  
	    		  fwrite.append(" lhs = "+lhs+",")
	    		  
	    		  XMLwriter.append("\n					<lhs name = \""+lhs+"\"/>")
	    		  
	    		  val start:Int=jimple_stmt.indexOf("<")
	    		  var i=0
	    		  var end:Int=0
	    		  for(i<-0 to jimple_stmt.length()-1 )
	    		  {
	    			  if(i>1 && jimple_stmt.charAt(i-1)==')' && jimple_stmt.charAt(i)=='>')
	    				  end=i
	    		  }
	    		  var invokation_var:String=jimple_stmt.substring(equal_pos+2+len+1, start-1)
	    		  
	    		  //print(" invokation object = "+invokation_var+",")	    		  
	    		  fwrite.append(" invokation object = "+invokation_var+",")	    		  
	    		  XMLwriter.append("\n					<invokation object = \""+invokation_var+"\"/>")
	    		  
	    		  //st
	    		  val delmtr:Int=jimple_stmt.indexOf(":")
	    		  val class_name:String=jimple_stmt.substring(start+1, delmtr)
	    		  
	    		  //print(" class = "+class_name+",")	    		  
	    		  fwrite.append(" class = "+class_name+",")	    		  
	    		  XMLwriter.append("\n					<class name = \""+class_name+"\"/>")
	    		  
	    		  val full_func:String=jimple_stmt.substring(delmtr+2, end)
	    		  val ret_type:String=full_func.substring(0, full_func.indexOf(" "))
	    		  val fun_name:String=full_func.substring(full_func.indexOf(" ")+1, full_func.length())
	    		  var close_brac_pos:Int=0
	    		  
	    		  for(i<-0 to fun_name.length()-1)
	    		  {
	    			  if(fun_name.charAt(i)==')')
	    				  close_brac_pos=i
	    		  }
	    		  ////print("close brac pos"+close_brac_pos)
	    		  var func_name_only:String=fun_name.substring(0,fun_name.indexOf("(")+1)
	    		 
	    		  //print(" function name = "+func_name_only+"),")
	    		  //print(" return type = "+ret_type+",")
	    		 
	    		  fwrite.append(" function name = "+func_name_only+"),")
	    		  fwrite.append(" return type = "+ret_type+",")
	    		  
	    		    //< check start
	    		   if(func_name_only.contains("<")|| func_name_only.contains("<"))
	    		  {
	    		    XMLwriter.append("					<function name = \"")
	    		    for(i <-0 to func_name_only.length()-1)
	    		    {
	    		      if(func_name_only.charAt(i)=='<' || func_name_only.charAt(i)=='>')
	    		      {
	    		        //XMLwriter.append()
	    		      }
	    		      else
	    		        XMLwriter.append(func_name_only.charAt(i))
	    		        
	    		    }
	    		    XMLwriter.append(")\"/>")
	    		  }
	    		  else
	    		  {
	    			  XMLwriter.append("\n					<function name = \""+func_name_only+")\"/>")
	    		  }
	    		  XMLwriter.append("\n					<return type = \""+ret_type+"\"/>\n")
	    		  //< check end
	    		  
	    		  var parameter_list:String=fun_name.substring(fun_name.indexOf("(")+1,close_brac_pos)
	    		  if(parameter_list.length()!=0)
	    		  {
	    			  //print(" parameter types = "+parameter_list+",")	    			  
	    			  fwrite.append(" parameter types = "+parameter_list+",")	    			  
	    			  XMLwriter.append("\n					<parameter_internal types = \""+parameter_list+"\"/>")
	    	 
	    			  var arg:String=jimple_stmt.substring(end+2, jimple_stmt.length()-1)
	    			  	    			  
	    			  //for special query string having \, : etc
	    			  //println("arg="+arg)
	    			  if(arg.contains("\""))
	    			  {
	    			    var arg_array:Array[String]=delemeterString(arg)
	    			    for(i<-0 to arg_array.length-1)
	    			    {
	    			      if(arg_array(i).charAt(0)=='\"' && arg_array(i).charAt(arg_array(i).length()-1)=='\"')
	    			        arg_array(i)="_constant"
	    			    }
	    			    arg=new String("")
	    			    for(i<-0 to arg_array.length-1)
	    			    {
	    			      if(i==arg_array.length-1)
	    			        arg=arg.concat(arg_array(i))
	    			      else
	    			        arg=arg.concat(arg_array(i)).concat(",")
	    			    } 
	    			  }
	    			  //end
	    			    
	    			  //arg="<![CDATA[".concat(arg).concat("]]")
	    			  
	    			  //print(" arg = "+arg+",")
	    			  
	    			  fwrite.append(" arg = "+arg+",")
	    			 
	    			  //arg processing start
	    			   if(arg.contains("\""))
	    			  {
	    				  if(arg.compareTo("\"\"")==0)
	    					  XMLwriter.append("\n					<arg><![CDATA[-empty string-]]></arg>")
	    				  else
	    				  {
	    				    XMLwriter.append("\n					<arg><![CDATA["+arg.replaceAll("\"", "")+"]]></arg>")
	    				  }
	    			  }
	    			  else
	    				XMLwriter.append("\n					<arg><![CDATA["+arg+"]]></arg>")
	    			//arg processing end
	    		  }
	    		  else
	    		  {
	    			  //print(" parameter types = void,")
	    			  fwrite.append(" parameter types = void,")
	    		  
	    			  //print(" arg = void,")
	    			  fwrite.append(" arg = void,")
	    			  
	    			  XMLwriter.append("					<parameter_internal types = \"void\"/>")
	    			  XMLwriter.append("\n					<arg><![CDATA[void]]></arg>")
	    		  }
	    	//end
	        }
	    	//method calling
	    	else
	    	{	    	  
	    		  val start:Int=jimple_stmt.indexOf("<")
	    		  var i=0
	    		  var end:Int=0
	    		  for(i<-0 to jimple_stmt.length()-1 )
	    		  {
	    			  if(i>0 && jimple_stmt.charAt(i-1)==')' && jimple_stmt.charAt(i)=='>')
	    				  end=i
	    		  }
	    		  var invokation_var:String=jimple_stmt.substring(len+1, start-1)
	    		  
	    		  //print(" invokation object = "+invokation_var+",")
	    		  
	    		  fwrite.append(" invokation object = "+invokation_var+",")
	    		  
	    		  XMLwriter.append("\n					<invokation object = \""+invokation_var+"\"/>")
	    		  //st
	    		  val delmtr:Int=jimple_stmt.indexOf(":")
	    		  val class_name:String=jimple_stmt.substring(start+1, delmtr)
	    		  
	    		  //print(" class = "+class_name+",")	    		  
	    		  fwrite.append(" class = "+class_name+",")	    		 
	    		  XMLwriter.append("\n					<class name = \""+class_name+"\"/>")
	    		  
	    		  val full_func:String=jimple_stmt.substring(delmtr+2, end)
	    		  val ret_type:String=full_func.substring(0, full_func.indexOf(" "))
	    		  var fun_name:String=full_func.substring(full_func.indexOf(" ")+1, full_func.length())
	    		  var close_brac_pos:Int=0
	    		  
	    		  for(i<-0 to fun_name.length()-1)
	    		  {
	    			  if(fun_name.charAt(i)==')')
	    				  close_brac_pos=i
	    		  }
	    		  
	    		  ////print("close brac pos"+close_brac_pos)
	    		  var func_name_only:String=fun_name.substring(0,fun_name.indexOf("(")+1)
	    		  
	    		  //print(" function name = "+func_name_only+"),")
	    		  //print(" return type = "+ret_type+",")
	    		  
	    		  fwrite.append(" function name = "+func_name_only+"),")
	    		  fwrite.append(" return type = "+ret_type+",")
	    		  
	    		  if(func_name_only.contains("<")|| func_name_only.contains("<"))
	    		  {
	    		    XMLwriter.append("\n					<function name = \"")
	    		    for(i <-0 to func_name_only.length()-1)
	    		    {
	    		      if(func_name_only.charAt(i)=='<' || func_name_only.charAt(i)=='>')
	    		      {
	    		        //XMLwriter.append()
	    		      }
	    		      else
	    		        XMLwriter.append(func_name_only.charAt(i))
	    		        
	    		    }
	    		    XMLwriter.append(")\"/>")//")\"/>")
	    		  }
	    		  else
	    		  {
	    			  XMLwriter.append("\n					<function name = \""+func_name_only+")\"/>")
	    		  }
	    			  XMLwriter.append("\n					<return type = \""+ret_type+"\"/>")
	    			  
	    	      	    		  
	    		  var parameter_list:String=fun_name.substring(fun_name.indexOf("(")+1,close_brac_pos)
	    		 
	    		  //println(fun_name)
	    		  if(fun_name.contains("(\"<"))
	    		  {
	    		    
	    		  }
	    		  if(parameter_list.length()!=0)
	    		  {
	    			  //print(" parameter types = "+parameter_list+",")
	    			  fwrite.append(" parameter types = "+parameter_list+",")
	    			  XMLwriter.append("\n					<parameter_internal types = \""+parameter_list+"\"/>\n")
	    	 
	    			  var arg:String=jimple_stmt.substring(end+2, jimple_stmt.length()-1)
	    			  
	    			  //for special query string having \, : etc
	    			  //println("virtualinvoke arg= "+arg)
	    			  if(arg.contains("\""))
	    			  {
	    			    var arg_array:Array[String]=delemeterString(arg)
	    			    for(i<-0 to arg_array.length-1)
	    			    {
	    			      if(arg_array(i).charAt(0)=='\"' && arg_array(i).charAt(arg_array(i).length()-1)=='\"')
	    			        arg_array(i)="_constant"
	    			    }
	    			    arg=new String("")
	    			    for(i<-0 to arg_array.length-1)
	    			    {
	    			      if(i==arg_array.length-1)
	    			        arg=arg.concat(arg_array(i))
	    			      else
	    			        arg=arg.concat(arg_array(i)).concat(",")
	    			    } 
	    			  }
	    			  //end
	    			  
	    			  //arg="<![CDATA[".concat(arg).concat("]]")
	    			  
	    			  //print(" arg = "+arg+",")
	    			  
	    			  fwrite.append(" arg = "+arg+",")
	    			  
	    			  //arg processing start
	    			   if(arg.contains("\""))
	    			  {
	    				  if(arg.compareTo("\"\"")==0)
	    					  XMLwriter.append("\n					<arg><![CDATA[-empty string-]]></arg>")
	    				  else
	    				  {
	    				    XMLwriter.append("\n					<arg><![CDATA["+arg.replaceAll("\"", "")+"]]></arg>")
	    				  }
	    			  }
	    			  else
	    				XMLwriter.append("\n					<arg><![CDATA["+arg+"]]></arg>")
	    			//arg processing end
	    		  }
	    		  else
	    		  {
	    			  //print(" parameter types = void,")
	    			  fwrite.append(" parameter types = void,")
	    		  
	    			  //print(" arg = void,")
	    			  fwrite.append(" arg = void,")
	    			  
	    			  XMLwriter.append("					<parameter_internal types = \"void\"/>")
	    			  XMLwriter.append("\n					<arg><![CDATA[void]]></arg>")
	    		  }  	    	  
	    	}
	    	
	    	//print(" >")
	    	fwrite.append(">")
	      }
	      //for special invoke statement
	     
	      if(jimple_stmt.contains("specialinvoke"))
	      {
	    	  var str:String="specialinvoke"
	          //print("\n          <")
	    	  fwrite.append("\n          <")
	    	  XMLwriter.append("\n					<statement type =\"specialinvoke\"/>")
	    	  var len:Int=str.length()	
	    	  
	    	  val start:Int=jimple_stmt.indexOf("<")
	    		  var i=0
	    		  var end:Int=0
	    		  
	    		  for(i<-0 to jimple_stmt.length()-1 )
	    		  {
	    			  if(jimple_stmt.charAt(i)=='>')
	    				  end=i
	    		  }
	    	  
	    		  var invokation_var:String=jimple_stmt.substring(len+1, start-1)
	    		  
	    		  //print(" invokation object = "+invokation_var+",")	    		  
	    		  fwrite.append(" invokation object = "+invokation_var+",")	    		  
	    		  XMLwriter.append("\n					<invokation object = \""+invokation_var+"\"/>\n")
	    		  
	    		  //st
	    		  val delmtr:Int=jimple_stmt.indexOf(":")
	    		  val class_name:String=jimple_stmt.substring(start+1, delmtr)
	    		  
	    		  //print(" class = "+class_name+",")
	    		  
	    		  fwrite.append(" class = "+class_name+",")
	    		  
	    		  XMLwriter.append("					<class name = \""+class_name+"\"/>\n")
	    		  
	    		  val full_func:String=jimple_stmt.substring(delmtr+2, end)
	    		  val ret_type:String=full_func.substring(0, full_func.indexOf(" "))
	    		  val fun_name:String=full_func.substring(full_func.indexOf(" ")+1, full_func.length())
	    		  var close_brac_pos:Int=0
	    		  
	    		  for(i<-0 to fun_name.length()-1)
	    		  {
	    			  if(fun_name.charAt(i)==')')
	    				  close_brac_pos=i
	    		  }
	    		  
	    		  ////print("close brac pos"+close_brac_pos)
	    		  var func_name_only:String=fun_name.substring(0,fun_name.indexOf("(")+1)
	    		  
	    		  //print(" function name = "+func_name_only+"),")
	    		  //print(" return type = "+ret_type+",")
	    		  
	    		  //< check start
	    		   if(func_name_only.contains("<")|| func_name_only.contains("<"))
	    		  {
	    		    XMLwriter.append("					<function name = \"")
	    		    for(i <-0 to func_name_only.length()-1)
	    		    {
	    		      if(func_name_only.charAt(i)=='<' || func_name_only.charAt(i)=='>')
	    		      {
	    		        //XMLwriter.append()
	    		      }
	    		      else
	    		        XMLwriter.append(func_name_only.charAt(i))
	    		        
	    		    }
	    		    XMLwriter.append(")\"/>")
	    		  }
	    		  else
	    		  {
	    			  XMLwriter.append("\n					<function name = \""+func_name_only+")\"/>")
	    		  }
	    		  XMLwriter.append("\n					<return type = \""+ret_type+"\"/>\n")
	    		  //< check end
	    		  
	    		  fwrite.append(" function name = "+func_name_only+"),")
	    		  fwrite.append(" return type = "+ret_type+",")
	    		  
	    		  //XMLwriter.append("					<function name = \""+func_name_only+")\"/>\n")
	    		  //XMLwriter.append("					<return type = \""+ret_type+"\"/>\n")
	    		  
	    		  var parameter_list:String=fun_name.substring(fun_name.indexOf("(")+1,close_brac_pos)
	    		  
	    		  if(parameter_list.length()!=0)
	    		  {
	    			  //print(" parameter types = "+parameter_list+",")
	    			  fwrite.append(" parameter types = "+parameter_list+",")
	    			  XMLwriter.append("					<parameter_internal types = \""+parameter_list+"\"/>\n")
	    	 
	    			  var arg:String=jimple_stmt.substring(end+2, jimple_stmt.length()-1)
	    			  //arg="<![CDATA[".concat(arg).concat("]]")
	    			  //print(" arg = "+arg+",")
	    			  fwrite.append(" arg = "+arg+",")
	    			  
	    			  //arg processing start
	    			   if(arg.contains("\""))
	    			  {
	    				  if(arg.compareTo("\"\"")==0)
	    					  XMLwriter.append("\n					<arg><![CDATA[-empty string-]]></arg>")
	    				  else
	    				  {
	    				    XMLwriter.append("\n					<arg><![CDATA["+arg.replaceAll("\"", "")+"]]></arg>")
	    				  }
	    			  }
	    			  else
	    				XMLwriter.append("\n					<arg><![CDATA["+arg+"]]></arg>")
	    			//arg processing end
	    		  }
	    		  else
	    		  {
	    			  //print(" parameter types = void,")
	    			  fwrite.append(" parameter types = void,")
	    		  
	    			  //print(" arg = void,")
	    			  fwrite.append(" arg = void,")
	    			  
	    			  XMLwriter.append("					<parameter_internal types = \"void\"/>")
	    			  XMLwriter.append("\n					<arg><![CDATA[void]]></arg>")
	    		  }
	    	
	    	  //print(" >")
	    	  
	    	  fwrite.append(">")
	      }
	       //for interface invoke statement
	      if(jimple_stmt.contains("interfaceinvoke"))
	      {
	          //print("\n          <")
	    	      fwrite.append("\n          <")
	    	      XMLwriter.append("\n					<statement type =\"interfaceinvoke\"/>")
	    	      var str:String="interfaceinvoke"
	    	      val len:Int=str.length()
	    	      
	        if(encounter_equal(jimple_stmt,"="))
	        {
	           var equal_pos:Int=jimple_stmt.indexOf("=")
	    		  ////print(equal_pos)
	    		  var lhs:String=jimple_stmt.substring(0, equal_pos-1)
	    		  
	    		  //print(" lhs = "+lhs+",")	    		  
	    		  fwrite.append(" lhs = "+lhs+",")	    		  	    		
	    		  XMLwriter.append("\n					<lhs name =\""+lhs+"\"/>")
	    		  
	    		  val start:Int=jimple_stmt.indexOf("<")
	    		  var i=0
	    		  var end:Int=0
	    		  
	    		  for(i<-0 to jimple_stmt.length()-1 )
	    		  {
	    			  if(jimple_stmt.charAt(i)=='>')
	    				  end=i
	    		  }
	    		  var invokation_var:String=jimple_stmt.substring(equal_pos+3+len, start-1)	    		  
	    		  //print(" invokation object = "+invokation_var+",")	    		  
	    		  fwrite.append(" invokation object = "+invokation_var+",")
	    		  
	    		  XMLwriter.append("\n					<invokation object = \""+invokation_var+"\"/>")
	    		  //val end:Int=jimple_stmt.indexOf(">")
	    		  val delmtr:Int=jimple_stmt.indexOf(":")
	    		  val class_name:String=jimple_stmt.substring(start+1, delmtr)
	    		  
	    		  //print(" class = "+class_name+",")
	    		  
	    		  fwrite.append(" class = "+class_name+",")
	    		  
	    		  XMLwriter.append("\n					<class name = \""+class_name+"\"/>")
	    		  
	    		  val full_func:String=jimple_stmt.substring(delmtr+2, end)
	    		  val ret_type:String=full_func.substring(0, full_func.indexOf(" "))
	    		  val fun_name:String=full_func.substring(full_func.indexOf(" ")+1, full_func.length())
	    		  var close_brac_pos:Int=0
	    		  
	    		  for(i<-0 to fun_name.length()-1)
	    		  {
	    			  if(fun_name.charAt(i)==')')
	    				  close_brac_pos=i
	    		  }
	    		  ////print("close brac pos"+close_brac_pos)
	    		  var func_name_only:String=fun_name.substring(0,fun_name.indexOf("(")+1)
	    		  
	    		  //print(" function name = "+func_name_only+"),")
	    		  //print(" return type = "+ret_type+",")
	    		  
	    		  fwrite.append(" function name = "+func_name_only+"),")
	    		  fwrite.append(" return type = "+ret_type+",")
	    		  
	    		    //< check start
	    		   if(func_name_only.contains("<")|| func_name_only.contains("<"))
	    		  {
	    		    XMLwriter.append("					<function name = \"")
	    		    for(i <-0 to func_name_only.length()-1)
	    		    {
	    		      if(func_name_only.charAt(i)=='<' || func_name_only.charAt(i)=='>')
	    		      {
	    		        //XMLwriter.append()
	    		      }
	    		      else
	    		        XMLwriter.append(func_name_only.charAt(i))
	    		        
	    		    }
	    		    XMLwriter.append(")\"/>")
	    		  }
	    		  else
	    		  {
	    			  XMLwriter.append("\n					<function name = \""+func_name_only+")\"/>")
	    		  }
	    		  XMLwriter.append("\n					<return type = \""+ret_type+"\"/>\n")
	    		  //< check end
	    		      
	    		  var parameter_list:String=fun_name.substring(fun_name.indexOf("(")+1,close_brac_pos)
	    		  
	    		  if(parameter_list.length()!=0)
	    		  {
	    			  //print(" parameter types = "+parameter_list+",")
	    			  
	    			  fwrite.append(" parameter types = "+parameter_list+",")
	    			  
	    			  XMLwriter.append("					<parameter_internal types = \""+parameter_list+"\"/>")
	    	 
	    			  var arg:String=jimple_stmt.substring(end+2, jimple_stmt.length()-1)
	    			  //arg="<![CDATA[".concat(arg).concat("]]")
	    			
	    			  //print(" arg = "+arg+",")
	    			  
	    			  fwrite.append(" arg = "+arg+",")
	    			  
	    			  //arg processing start
	    			   if(arg.contains("\""))
	    			  {
	    				  if(arg.compareTo("\"\"")==0)
	    					  XMLwriter.append("\n					<arg><![CDATA[-empty string-]]></arg>")
	    				  else
	    				  {
	    				    XMLwriter.append("\n					<arg><![CDATA["+arg.replaceAll("\"", "")+"]]></arg>")
	    				  }
	    			  }
	    			  else
	    				XMLwriter.append("\n					<arg><![CDATA["+arg+"]]></arg>")
	    			//arg processing end
	    		  }
	    		  else
	    		  {
	    			  //print(" parameter types = void,")
	    			  fwrite.append(" parameter types = void,")
	    		  
	    			  //print(" arg = void,")
	    			  fwrite.append(" arg = void,")
	    			  
	    			  XMLwriter.append("					<parameter_internal types = \"void\"/>")
	    			  XMLwriter.append("\n					<arg><![CDATA[void]]></arg>")
	    		  }
	        }
	        
	        else
	        {
	    	      //print("\n          <")
	    	      fwrite.append("\n          <")
	    	      //XMLwriter.append("\n					<statement type =\"interfaceinvoke\"/>")
	    	     
	    		  val start:Int=jimple_stmt.indexOf("<")
	    		  var i=0
	    		  var end:Int=0
	    		  for(i<-0 to jimple_stmt.length()-1 )
	    		  {
	    			  if(jimple_stmt.charAt(i)=='>')
	    				  end=i
	    		  }
	    		  var invokation_var:String=jimple_stmt.substring(len+1, start-1)
	    		  
	    		  //print(" invokation object = "+invokation_var+",")
	    		  
	    		  fwrite.append(" invokation object = "+invokation_var+",")
	    		  
	    		  XMLwriter.append("\n					<invokation object = \""+invokation_var+"\"/>\n")
	    		  
	    		  //st
	    		  val delmtr:Int=jimple_stmt.indexOf(":")
	    		  val interface_name:String=jimple_stmt.substring(start+1, delmtr)
	    		  
	    		  //print(" interface = "+interface_name+",")
	    		  
	    		  fwrite.append(" interface = "+interface_name+",")
	    		  XMLwriter.append("					<interface name= \""+interface_name+"\"/>\n")
	    		  
	    		  val full_func:String=jimple_stmt.substring(delmtr+2, end)
	    		  val ret_type:String=full_func.substring(0, full_func.indexOf(" "))
	    		  val fun_name:String=full_func.substring(full_func.indexOf(" ")+1, full_func.length())
	    		  var close_brac_pos:Int=0
	    		  
	    		  for(i<-0 to fun_name.length()-1)
	    		  {
	    			  if(fun_name.charAt(i)==')')
	    				  close_brac_pos=i
	    		  }
	    		  ////print("close brac pos"+close_brac_pos)
	    		  var func_name_only:String=fun_name.substring(0,fun_name.indexOf("(")+1)
	    		  
	    		  //print(" function name = "+func_name_only+"),")
	    		  //print(" return type = "+ret_type+",")
	    		  
	    		  fwrite.append(" function name = "+func_name_only+"),")
	    		  fwrite.append(" return type = "+ret_type+",")
	    		  
	    		  XMLwriter.append("					<function name = \""+func_name_only+")\"/>\n")
	    		  XMLwriter.append("					<return type = \""+ret_type+"\"/>\n")
	    		  
	    		  var parameter_list:String=fun_name.substring(fun_name.indexOf("(")+1,close_brac_pos)
	    		  
	    		  if(parameter_list.length()!=0)
	    		  {
	    			  //print(" parameter types = "+parameter_list+",")
	    			  fwrite.append(" parameter types = "+parameter_list+",")
	    			  XMLwriter.append("					<parameter_internal types = \""+parameter_list+"\"/>\n")
	    	 
	    			  var arg:String=jimple_stmt.substring(end+2, jimple_stmt.length()-1)
	    			  //arg="<![CDATA[".concat(arg).concat("]]")
	    			  //print(" arg = "+arg+",")
	    			  
	    			  fwrite.append(" arg = "+arg+",")
	    			  
	    			  //arg processing start
	    			   if(arg.contains("\""))
	    			  {
	    				  if(arg.compareTo("\"\"")==0)
	    					  XMLwriter.append("\n					<arg><![CDATA[-empty string-]]></arg>")
	    				  else
	    				  {
	    				    XMLwriter.append("\n					<arg><![CDATA["+arg.replaceAll("\"", "")+"]]></arg>")
	    				  }
	    			  }
	    			  else
	    				XMLwriter.append("\n					<arg><![CDATA["+arg+"]]></arg>")
	    			//arg processing end
	    		  }
	    		  else
	    		  {
	    			  //print(" parameter types = void,")
	    			  fwrite.append(" parameter types = void,")
	    		  
	    			  //print(" arg = void,")
	    			  fwrite.append(" arg = void,")
	    			  
	    			  XMLwriter.append("					<parameter_internal types = \"void\"/>")
	    			  XMLwriter.append("\n					<arg><![CDATA[void]]></arg>")
	    		  } 	    	  	
	    	
	    	//print(" >")
	    	fwrite.append(">")
	        }
	      }
	      XMLwriter.append("\n")
	      //println()
	      fwrite.append("\n")
	      
	      if(jimple_stmt.equals("nop"))
	      {
	        XMLwriter.append("					<statement type = \"nopstatement\"/>\n")
	      }
	      
	      if(jimple_stmt.contains("@parameter"))
	      {
	    	XMLwriter.append("					<statement type = \"parameterassign\"/>\n")
	      }
	      
	      if(jimple_stmt.contains("@this"))
	      {
	    	XMLwriter.append("					<statement type = \"currentobjectinvoke\"/>\n")
	    	if(jimple_stmt.contains("="))
	    	  {
	    		  var equal_pos:Int=jimple_stmt.indexOf("=")
	    		  ////print(equal_pos)
	    		  var lhs:String=jimple_stmt.substring(0, equal_pos-2)  //due to :=
	    		  //print(" lhs = "+lhs+",\n")
	    		  fwrite.append("		<lhs = "+lhs+">\n")  	    		
	    		  XMLwriter.append("					<lhs name =\""+lhs+"\"/>\n")
	    	  }        
	      }
	      
	      if(jimple_stmt.contains("return"))
	      {
	        XMLwriter.append("					<statement type = \"returnstatement\"/>\n")
	        
	        var return_var:String=""
	        if(jimple_stmt.contains(" "))
	        {
	        	return_var=jimple_stmt.substring(jimple_stmt.indexOf(" ")+1, jimple_stmt.length()).replaceAll("\"", "").replaceAll(">", "").replaceAll("<", "")	        	
	        }
	        else
	        {
	        	return_var="void"
	        }
	        
	        XMLwriter.append("					<return_var name = \""+return_var+"\"/>\n")
	      }
	      
	      //trace assign statement
	      if(!jimple_stmt.contains("interfaceinvoke") && !jimple_stmt.contains("staticinvoke") 
	          && !jimple_stmt.contains("virtualinvoke") && !jimple_stmt.contains("specialinvoke") 
	          && !jimple_stmt.contains("return") && !jimple_stmt.contains("@parameter") 
	          && !jimple_stmt.contains(" goto label") && !jimple_stmt.equals("nop") && !jimple_stmt.contains("@parameter")
	          && !jimple_stmt.contains("@this"))
	      {
	        XMLwriter.append("					<statement type = \"assignstatement\"/>\n")
	        if(jimple_stmt.contains(".<") && jimple_stmt.contains(">"))
	        {
	          if(jimple_stmt.indexOf(".<")<jimple_stmt.indexOf(">"))
	          {
	            var objectname:String=jimple_stmt.substring(0,jimple_stmt.indexOf(".<"))
	            XMLwriter.append("					<invokeObject name = \""+objectname+"\"/>\n")
	          }
	        }
	        if(jimple_stmt.contains("<") && jimple_stmt.contains(">"))
	        {
	          var eq_pos:Int=0
	          for(i<-0 to jimple_stmt.length()-1)
	          {
	            if(jimple_stmt.charAt(i)=='=')
	              eq_pos=i
	          }
	          
	           var brac_pos:Int=0
	          for(i<-0 to jimple_stmt.length()-1)
	          {
	            if(jimple_stmt.charAt(i)=='>')
	              brac_pos=i
	          }
	           if(eq_pos>brac_pos)
	           {
	        	   var lhs:String=jimple_stmt.substring(jimple_stmt.indexOf(":")+2, jimple_stmt.indexOf(">"))
	        	 
	        	   if(lhs.contains(" "))
	        	   {
	        		   lhs=lhs.substring(lhs.indexOf(" "), lhs.length())
	        	   }
	           	   //print(" lhs = "+lhs+",\n")
	           	   fwrite.append("		<lhs = "+lhs.replaceAll(" ", "")+">\n")  	    		
	           	   XMLwriter.append("					<lhs name =\""+lhs.replaceAll(" ", "")+"\"/>\n")
	           }
	           else
	           {
	             var lhs:String=jimple_stmt.substring(0,eq_pos-1)
	             fwrite.append("		<lhs = "+lhs.replaceAll(" ", "")+">\n")  	    		
	           	 XMLwriter.append("					<lhs name =\""+lhs.replaceAll(" ", "")+"\"/>\n")
	           }
	        }
	        else if(encounter_equal(jimple_stmt,"="))
	    	  {
	    		  var equal_pos:Int=jimple_stmt.indexOf("=")
	    		  ////print(equal_pos)
	    		  var lhs:String=jimple_stmt.substring(0, equal_pos-1)
	    		  //print(" lhs = "+lhs+",\n")
	    		  fwrite.append("		<lhs = "+lhs+">\n")  	    		
	    		  XMLwriter.append("					<lhs name =\""+lhs+"\"/>\n")
	    	  }
	        
	      }
     	    	//in out traces
    	 		var inlist=stmt \\ "in"    	 		
    	 		if(inlist.length!=0)
    	 		{
    	 		  
    	 			XMLwriter.append("					<in count = \""+inlist.length+"\">\n")
    	 			//print("          ==in list : [")
    	 			fwrite.append("          ==in list : [") 
    	 			
    	 			inlist.foreach(in=>
    	 			{
    	 			  //print(in \ "@local"+",")
    	 			  fwrite.append(in \ "@local"+",")
    	 			  XMLwriter.append("						<in_var name = \""+in \ "@local"+"\"/>\n")
    	 			})
    	 			//println("]")
    	 			fwrite.append("]\n")
    	 			XMLwriter.append("					</in>\n")
    	 			
    	 		}	      		
    	 		var outlist=stmt \\ "out"
    	 		
    	 		if(outlist.length!=0)
    	 		{
    	 			XMLwriter.append("					<out count = \""+outlist.length+"\">\n")
    	 			//print("          ==out list : [")
    	 			fwrite.append("          ==out list : [")
    	 			
    	 			outlist.foreach(out=>
    	 			{
    	 			  //print(out \ "@local"+",")
    	 			  fwrite.append(out \ "@local"+",")
    	 			  XMLwriter.append("						<out_var name = \""+out \ "@local"+"\"/>\n")
    	 			})
    	 			
    	 			//println("]")
    	 			fwrite.append("]\n")
    	 			XMLwriter.append("					</out>\n")
    	 		}
	      		
    	 		XMLwriter.append("				</jimple_stmt>\n")
    	 		
	    })//statement list
	    ////println(labelmap)
	    
	    //println("----End of jimple statement----");
	    fwrite.append("----End of jimple statement----\n")
	    
	    
	    XMLwriter.append("			</jimple_statement>\n")
	    var exception_count_string:String=l \\ "exceptions" \ "@count" text;
	    var exception_count=Integer.parseInt(exception_count_string)
	    XMLwriter.append("			<exceptions count = \""+exception_count_string+"\">\n")
	    fwrite.append("exception count = \""+exception_count_string+"\">\n")
	    ////print(exception_count+"@@@@@@@@@")
	    var exp=l \\ "exceptions" \\ "exception"
	    exp.foreach(exp=> 
	    {
	     XMLwriter.append("				<exception  method =\""+exp \"@method"+"\" type = \""+exp \"@type"+"\" begin = \""+exp \\ "begin" \"@label"+"\" end = \""+exp \\ "end" \ "@label"+"\" handler = \""+exp \ "handler" \ "@label"+"\"/>\n")
	    })
	    
	    XMLwriter.append("			</exceptions>\n")
	    XMLwriter.append("		</method>\n")
	    
	  })//method
	  
	  XMLwriter.append("	</class_i>\n")
	  
     })//class
     
   XMLwriter.append("</cfg_generator>")
   fwrite.close()
   XMLwriter.close()
   print("\n ===== write complete ======")
   
   //repair XML
   var fw_tmp:java.io.FileWriter=new FileWriter(loc+"XMLgen\\"+filename+"_gen.TMP");
   for(temp <- Source.fromFile(loc+"XMLgen\\"+filename+out_extension).getLines())
   {
     if(!temp.contains("&amp;"))
       fw_tmp.append(temp.replaceAll("&", "&amp;")+"\n")
     
     else
       fw_tmp.append(temp+"\n")
   }
   fw_tmp.close();
   
   var fw_chk:java.io.FileWriter=new FileWriter(loc+"XMLgen\\"+filename+out_extension);
   for(temp <- Source.fromFile(loc+"XMLgen\\"+filename+"_gen.TMP").getLines())
   {
     fw_chk.append(temp+"\n")
   }
   fw_chk.close();
   
	})
  }
}