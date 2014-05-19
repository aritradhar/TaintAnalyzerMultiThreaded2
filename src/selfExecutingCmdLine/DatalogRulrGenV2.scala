package selfExecutingCmdLine

//author: Aritra Dhar
//Intern
//Accenture Technology Labs
//version 1.6

import java.io._
import scala.xml.XML
import javax.xml.soap.Text
import java.util._
import scala.Mutable
import scala.collection.JavaConversions._
import scala.collection.immutable.Set
import scala.io.Source

class DatalogRulrGenV2
{

  def main(ar:Array[String]) =
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
  var dlmtr:String="_"
   
    
  def isNum(S:String):Boolean =
  {
    var len:Int=S.length()
    var i:Int=0
    var counter:Int=0
    for(i <- 0 to len-1)
    {
      if(S.charAt(i).isDigit)
        counter=counter+1
      if(S.charAt(i)=='.')
        counter=counter+1
      if((S.charAt(i)=='F' || S.charAt(i)=='f') && S.contains("."))
        counter=counter+1
    }
    if(counter==len)
      return true
    else
      return false
  }
  
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
      println("filename="+filename_s)
    }
    //print(loc_pos)
  }
  filenameGenerator(tempFile)
  println("loc="+loc)
  
  var filename= Set(filename_s)
  
  
  var arr_arg:Array[Int]=new Array[Int](100)
  
  def arg_counter(arg:String):Int =
  {
    if(arg.length()==0 || arg.equals("void"))
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
         {
           arr_arg(arg_count)=i
           arg_count=arg_count+1
         }
       }
    }
    return (arg_count+1)
  }
	
 
  
	def loadXML(fileName:String) = 
    using(new FileInputStream(fileName))
    {
	  fis => using(new InputStreamReader(fis))
	  {
	    isr => XML.load(isr)
	  }
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
  	
		
  	 //var filename=Set("probCalc")
  	    
	 filename.foreach(filename=>{
  	var filename_datalog:String=filename+"_datalog.dl"
  	  var extension:String="_gen.xml"
  	  var temp_str:String="#temp"
  	  var temp_cout:Int=0
  	  
  	  val XMLfile=loadXML(loc+"XMLgen\\"+filename+extension)
  	  val fname_datalog:java.io.File=new File(loc+"datalog\\")
  	  if(!fname_datalog.exists())
  	  fname_datalog.mkdir();
  	  var datalog:java.io.FileWriter=new FileWriter(loc+"datalog\\"+filename_datalog)
  	  val datalog_path:java.io.FileWriter=new FileWriter("fact_path.txt")
  	  datalog_path.append(loc+"datalog\\"+filename_datalog)
  	  datalog_path.close()
  	  
  	  print("\n\n---- handling file : "+filename+" ----\n\n")
  	  datalog.append("//----datalog facts----\n")
  	  datalog.append("//----Author : Aritra Dhar----\n")
  	  datalog.append("//Intern\n")
  	  datalog.append("//Accenture Technology labs\n")
  	  datalog.append("//----version : 1.6----\n")
  	  val classlist=XMLfile \\ "class_i"
  	  var i:Int=0
  	  
  	  classlist.foreach(classlist => 
  	  {
  	    //print(classlist \ "@name")
  	    datalog.append("//Class name : "+classlist \ "@name"+"\n")
  	    val methodlist=classlist \\ "method" 
  	    
  	    var fieldlist=classlist \\ "field"
  	    
  	   datalog.append("//method name:static field\n")
  	    datalog.append("//class fields desc \n")
  	    fieldlist.foreach(fld =>
  	      {
  	          var fld_name:String=fld \ "@name" text;
  	          var fld_type:String=fld \ "@type" text;
  	          
  	          //print("SetType('"+fld_name+"','"+fld_type+"').\n")
  	    	  datalog.append("SetType('"+fld_name+"','"+fld_type+"').\n")
  	      }
  	        )
  	        
  	         datalog.append("\n\n")
  	    
  	    methodlist.foreach(m => 
  	    {
  	      var ji:Int=0
  	      var jump_z:Array[Int] = new Array[Int](10000000)
  	      var localvar_list=m \\ "local_var" \\ "local"
  	      
  	      var localmap:java.util.Map[String,String]=new java.util.HashMap()
  	      var local_defmap:java.util.Map[String,String]=new java.util.HashMap()
  	      
  	      localvar_list.foreach(loc =>
  	        {
  	          var loc_name:String=loc \ "@name" text;
  	          var loc_type:String=loc \ "@type" text;
  	          var loc_def:String=loc \ "@def" text;
  	          localmap.put(loc_name,loc_type)
  	          local_defmap.put(loc_name, loc_def)
  	        })
  	        
  	      ////print(localmap.entrySet())
  	        
  	      var goto_flag:Int=0
  	      var j:Int=0
  	      var m_name:String=m \ "@name" text;
  	      //print("\n\nmethod name : "+ m \ "@name")
  	       	  
  	      datalog.append("//method name : "+ m \ "@name"+"\n")
  	      datalog.append("\nMethodName('"+m \ "@name"+"').")
  	      
  	      //var jimplelist=methodlist \\ "jimple_statement"
  	      var nodeList:java.util.Map[String,String]=new java.util.HashMap()
  	      var stmt_id_set:java.util.SortedSet[Int]=new java.util.TreeSet()
  	      //jimplelist.foreach(jm=>{
  	      
  	        var jimple_stmtlist=m \\ "jimple_stmt"
  	        
  	        var id_jimple:java.util.Map[String,String]=new HashMap()
  	       
  	        jimple_stmtlist.foreach(stmt => {
  	           	          
  	          var stmt_id:String=stmt \ "@stmt_id" text;  	           	          
  	          var stmt_line:String=(stmt.text) trim()
  	          var arg_name_p:String=stmt \\ "arg" \ "@name" text;
  	          id_jimple.put(stmt_id, arg_name_p )
  	          ////print(stmt_id+"  : "+stmt_line+"\n")
  	          stmt_id_set.add(java.lang.Integer.parseInt(stmt_id))
  	          nodeList.put(stmt_id, stmt_line)
  	          var jump=stmt \\ "jump"
  	          
  	          
  	          //space in front and back to goto to diffentiate it from constant name 
  	          if(stmt_line.contains("goto label") && !stmt_line.contains("tableswitch") && !stmt_line.contains("lookupswitch")) 
  	          {
  	            j=j+1
  	            goto_flag=1
  	          }  	            	         
  	          ////printf("!!!!"+ji)
  	          
  	          if(stmt_line.contains("goto label") && !stmt_line.contains("tableswitch") && !stmt_line.contains("lookupswitch")) 
  	          {
  	            //debug
  	            //print(stmt_line)
  	            var tgt:String=jump \ "@target" text;
  	            var tgt_stmnt_id:Int=java.lang.Integer.parseInt(tgt.substring(tgt.indexOf(":")+2, tgt.size))
  	            //debug
  	            //println("///"+tgt_stmnt_id)
  	            jump_z(ji)=java.lang.Integer.parseInt(stmt_id)
  	            ji=ji+1
  	            jump_z(ji)=tgt_stmnt_id
  	            ji=ji+1
  	              //var tgt_stmnt:String=(tgt.substring(tgt.indexOf(":")+2, tgt.size))
  	            
  	        //print ("\n\\\\goto    "+ java.lang.Integer.parseInt(stmt_id)+"------>" +tgt_stmnt_id+" "+"|||"+ji)
  	           ////print("j :"+j)
  	          }	          
  	        })//jimple stmt
  	      //})//jimple list
  	        var z:Array[Int] = new Array[Int](stmt_id_set.size())
  	        var it:java.util.Iterator[Int]=stmt_id_set.iterator()
  	        i=0
  	        while(it.hasNext())
  	        {
  	          ////print(it.next())
  	          z(i)=it.next()
  	          i=i+1;
  	          ////print(it.next())
  	        }
  	     
  	      //Sequential Pred()
  	      for(i <- 0 to stmt_id_set.size()-2)
  	      {
  	        //print("\nPred('"+m_name+dlmtr+z(i)+"','"+m_name+dlmtr+z(i+1)+"').")
  	        datalog.append("\nPred('"+m_name+dlmtr+z(i)+"','"+m_name+dlmtr+z(i+1)+"').")
  	      }
  	      ////print("!!"+jump_z_cl(1))
  	      
  	      //Pred() due to fump and jump fallback
  	      if(goto_flag==1)
  	      {
  	    	  for(i <- 0 to ji-1)
  	    	  {
  	    		  if(i%2==0)
  	    		  {
  	    			  //print("\nPred('"+m_name+dlmtr+jump_z(i)+"','"+m_name+dlmtr+jump_z(i+1)+"').")
  	    			  datalog.append("\nPred('"+m_name+dlmtr+jump_z(i)+"','"+m_name+dlmtr+jump_z(i+1)+"').")
  	    		  }
  	    	  }
  	      }
  	      //Pred due to switch case statement  	      
  	      //trace Pred() caused by try-catch-finally block
  	       
  	       val labelmap: java.util.Map[String, String] = new java.util.HashMap()
  	       
  	       jimple_stmtlist.foreach(stmt => 
  	         {
  	           var lb_id:String = stmt \ "@label_id" text;
  	           var stmt_id:String = stmt \ "@stmt_id" text;
  	           if(!labelmap.containsKey(lb_id))
  	           {
  	             labelmap.put(lb_id, stmt_id)
  	           }  
  	           var stmt_type:String=stmt \\ "statement" \ "@type" text;  	           
  	           if(stmt_type.compareTo("switchstatement")==0)
  	           {
  	        	    var curr_id:String=stmt \ "@stmt_id" text;
  	           		var case_jump_node=stmt \\ "case_jump"
  	           		case_jump_node.foreach(node=>
  	           		  {
  	           			var target:String=node \ "@target" text;
  	           		    var target_id:String=target.substring(target.indexOf(":")+1, target.length())
  	           		    datalog.append("\nPred('"+m_name+dlmtr+curr_id+"','"+m_name+dlmtr+target_id+"').")
  	           		  })
  	           }
  	         })
  	       var exception_list=m \\ "exception"
  	       exception_list.foreach(ex => 
  	       {
  	    	   var begin_lb:String=ex \ "@begin" text;
  	    	   var end_lb:String=ex \ "@end" text;
  	    	   var begin_stmt=labelmap.get(begin_lb)
  	    	   var end_stmt=labelmap.get(end_lb)
  	    	   //print("\nPred('"+m_name+dlmtr+begin_stmt+"','"+m_name+dlmtr+end_stmt+"').")
  	    	   datalog.append("\nPred('"+m_name+dlmtr+begin_stmt+"','"+m_name+dlmtr+end_stmt+"').")
  	       })
  	      
  	      //set type facts for variable name -> variable type
  	      
  	       localmap.keySet().foreach(lm=>{
  	        //print("\nSetType('"+m_name+dlmtr+local_defmap.get(lm)+"','"+lm+"','"+localmap.get(lm)+"').")
  	        datalog.append("\nSetType('"+m_name+dlmtr+local_defmap.get(lm)+"','"+lm+"','"+localmap.get(lm)+"').")
  	       })
  	      /*
  	       localmap.foreach(lm=>{
  	        //print("\nSetType('"+lm._1+"','"+lm._2+"').")
  	        datalog.append("\nSetType('"+lm._1+"','"+lm._2+"').")
  	      })*/
  	      
  	     
  	      goto_flag=0
  	        ////print("j :"+j)
  	        ////print(it.size)
  	      
  	      //analyze static invoke part for Assignment() fact
  	      
  	      jimple_stmtlist.foreach(stmt => 
  	      {
  	          
  	          var stmt_id:String=stmt \ "@stmt_id" text
  	          var stmt_line:String=(stmt.text) trim()
  	          
  	          //tracing assignment statement
  	          var statement_type:String=stmt \\ "statement" \ "@type" text;
  	          var class_name:String=stmt \\ "class" \ "@name" text;
  	          var function_name:String=stmt \\ "function" \ "@name" text;
                  var object_name:String=stmt \\ "invokeObject" \ "@name" text;
                  if(object_name.length==0)
                    object_name="this"
  	          //drop () after the function anme to match with the signature in the knowledge-base
  	          if(function_name.length()>0)
  	        	  function_name=function_name.substring(0, function_name.indexOf("()"))
  	          var arg_name_rep_node=stmt \\ "arg"
  	          var arg_name_rep:String=(arg_name_rep_node.text)
                  
                  var in_live_var:java.util.Map[Int,String]=new java.util.HashMap()
                  var out_live_var:java.util.Map[Int,String]=new java.util.HashMap()
  	          
                  var in_list=stmt \\ "in_var"
                  var out_list=stmt \\ "out_var"
                  
                  var in_c:Int=0
                  var out_c:Int=0
                  
                  in_list.foreach(in_list=>
                    {
                      var in_name:String=in_list \"@name" text;
                      in_live_var.put(in_c,in_name)
                      in_c=in_c+1
                    })
                  
                  out_list.foreach(out_list=>
                   {
                      var out_name:String=out_list \"@name" text;
                      out_live_var.put(out_c,out_name)
                      out_c=out_c+1
                    })
                  
                  //println(in_live_var)
                  //println(out_live_var)
                    
  	          //if(arg_name_rep.contains("\"") || arg_name_rep.contains("'"))
  	            //arg_name_rep="_constant"
  	             
  	          var arg_name:String=arg_name_rep.replaceAll("\"", "").replaceAll("'","")
  	          var lhs_name_p:String=stmt \\ "lhs" \ "@name" text;
  	          
  	          if(lhs_name_p.length()!=0)
  	          {
  	             if(lhs_name_p.contains("\"") || lhs_name_p.contains("'") || isNum(lhs_name_p) )
  	               lhs_name_p="_constant"
  	          }
  	          
  	          var lhs_name:String=new String("")
  	          if(lhs_name_p.length()!=0)
  	            lhs_name=lhs_name_p.replaceAll("\"", "").replaceAll("'","")
  	          var arg_count:Int=arg_counter(arg_name)
  	          //println(stmt_id+" "+lhs_name_p)
  	          def argument()=
  	          {
  	            if(arg_count==1)
  	             {
  	            	 ////print("\n"+arg_name)
  	            	 //print("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','1','"+arg_name+"').")
  	            	if(arg_name.contains("\"") || arg_name.contains("'") || isNum(arg_name) )
  	            	  arg_name="_constant"
  	            	datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','1','"+arg_name+"').")
  	             } 
  	             
  	             if(arg_count>1)
  	             {
  	            	 ////print("\n"+arg_name.substring(0, arr_arg(0)))
  	            	 //print("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','1','"+arg_name.substring(0, arr_arg(0))+"').")
  	                 if(arg_name.substring(0, arr_arg(0)).contains("\"") || arg_name.substring(0, arr_arg(0)).contains("'") || isNum(arg_name.substring(0, arr_arg(0))) )
  	                	 datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','1','"+"_constant"+"').")
  	                 else
  	                	 datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','1','"+arg_name.substring(0, arr_arg(0))+"').")
  	             }
  	             for(i <-0 to arg_count-1-1)
  	             {
  	               ////print()
  	               if(i==arg_count-1-1)	
  	               {
  	                 if(arg_name.charAt(arr_arg(i)+1)==' ')
  	                 {
  	                	 ////print("\n"+arg_name.substring(arr_arg(i)+2,arg_name.length()))
  	                	 //print("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+arg_name.substring(arr_arg(i)+2,arg_name.length())+"').")
  	                	 if(arg_name.substring(arr_arg(i)+2,arg_name.length()).contains("\"") || arg_name.substring(arr_arg(i)+2,arg_name.length()).contains("'") || isNum(arg_name.substring(arr_arg(i)+2,arg_name.length())))
  	                		 datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+"_constant"+"').")
  	                	 else 
  	                		 datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+arg_name.substring(arr_arg(i)+2,arg_name.length())+"').")  	     
  	                 }
  	                 else
  	                 {
  	                	 ////print("<>")
  	                	 //print("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+arg_name.substring(arr_arg(i)+1,arg_name.length())+"').")
  	                	 //datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+arg_name.substring(arr_arg(i)+1,arg_name.length())+"').")
  	                   if(arg_name.substring(arr_arg(i)+1,arg_name.length()).contains("\"") || arg_name.substring(arr_arg(i)+1,arg_name.length()).contains("'") || isNum(arg_name.substring(arr_arg(i)+1,arg_name.length())))
  	                	   datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+"_constant"+"').")
  	                   else 
  	                	   datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+arg_name.substring(arr_arg(i)+1,arg_name.length())+"').") 
  	                 }
  	               }
  	               else
  	               {	
  	                 if(arg_name.charAt(arr_arg(i)+1)==' ')
  	                 {
  	                	 ////print("\n"+arg_name.substring(arr_arg(i)+2,arr_arg(i+1)))
  	                	 //print("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+arg_name.substring(arr_arg(i)+2,arr_arg(i+1))+"').")
  	                	 //datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+arg_name.substring(arr_arg(i)+2,arr_arg(i+1))+"').")
  	                	 if(arg_name.substring(arr_arg(i)+2,arr_arg(i+1)).contains("\"") || arg_name.substring(arr_arg(i)+2,arr_arg(i+1)).contains("'") || isNum(arg_name.substring(arr_arg(i)+2,arr_arg(i+1))))
  	                	   datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+"_constant"+"').")
  	                   else 
  	                	   datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+arg_name.substring(arr_arg(i)+2,arr_arg(i+1))+"').")
  	                 }
  	                 else
  	                 {
  	                	 ////print("<>")
  	                	 //print("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+arg_name.substring(arr_arg(i)+1,arr_arg(i+1))+"').")
  	                	 //datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+arg_name.substring(arr_arg(i)+1,arr_arg(i+1))+"').")
  	                   	 if(arg_name.substring(arr_arg(i)+1,arr_arg(i+1)).contains("\"") || arg_name.substring(arr_arg(i)+1,arr_arg(i+1)).contains("'") || isNum(arg_name.substring(arr_arg(i)+1,arr_arg(i+1))))
  	                	   datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+"_constant"+"').")
  	                     else 
  	                	   datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+(i+2)+"','"+arg_name.substring(arr_arg(i)+1,arr_arg(i+1))+"').")
  	                 }
  	               }
  	             }
  	          }
  	           
  	          def invoke_obj()
  	          {
  	            var invokation_object:String=stmt \\ "invokation" \ "@object" text;
  	            if(invokation_object.length()>0)
  	            {
  	              datalog.append("\nSetInvokeParam('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+0+"','"+invokation_object+"').")
  	            }
  	          }
  	          
  	          if(statement_type.equals("assignstatement") && stmt_line.contains("new "))
  	          {
  	              var next_stmt_arg:String=id_jimple.get((Integer.parseInt(stmt_id)+1).toString)
  	              var arg_count_curr=arg_counter(next_stmt_arg)
  	        	  //print("\nInvoke('"+m_name+dlmtr+(Integer.parseInt(stmt_id)+1).toString+"','"+"_constructor"+"','"+arg_count_curr+"','"+lhs_name+"').")
  	        	  datalog.append("\nInvoke('"+m_name+dlmtr+(Integer.parseInt(stmt_id)+1).toString+"','"+"_constructor"+"','"+arg_count_curr+"','"+lhs_name+"').")
  	        	  invoke_obj()
  	        	  argument()     	        	        	  
  	        	  //println(m_name+stmt_id+"  "+arg_count)
  	          }
  	          
  	           else if(statement_type.equals("assignstatement"))
  	          {
  	            ////print("<<<<<"+stmt_id+">>>>")   	            
  	            var lhs:String=stmt_line.substring(0,stmt_line.indexOf(" "))
  	            if(lhs.contains("\"") || lhs.contains("'") || isNum(lhs_name_p))
  	            lhs="_constant"
  	            var rhs_full:String=stmt_line.substring(stmt_line.indexOf(" ")+1,stmt_line.length())
  	            var last_space_index:Int=0
  	            for(i <- 0 to rhs_full.length()-1)
  	            {
  	              if(rhs_full.charAt(i)==' ')
  	                last_space_index=i
  	            }
  	            //var rhs:String=rhs_full.substring(rhs_full.indexOf(" ")+1, rhs_full.length()).replaceAll("\"", "").replaceAll("'","")
  	            var rhs:String=rhs_full.substring(last_space_index+1, rhs_full.length())
  	            if(rhs.contains("\"") || rhs.contains("'") || isNum(rhs))
  	            rhs="_constant"
  	            
  	            if(stmt_line.contains(":") && stmt_line.contains("<") && stmt_line.contains(">"))
  	            {
  	                lhs=lhs_name
  	            	//rhs=stmt_line.substring(stmt_line.indexOf("=")+2,stmt_line.length())
  	            	//println(rhs)
  	                if(rhs.contains(">"))
  	                	rhs=rhs.replaceAll(">", "")
  	                	
  	                if(rhs.contains("<") && rhs.contains(">"))
  	            	{
  	            	  var brac_pos:Int=0
  	            	  for(i<-0 to rhs.length()-1)
  	            	  {
  	            	    if(rhs.charAt(i)=='>')
  	            	      brac_pos=i
  	            	  }
  	            	  rhs=rhs.substring(rhs.indexOf(":"),brac_pos)
  	            	  var sp_pos:Int=0
  	            	  for(i <- 0 to rhs.length()-1)
  	            	  {
  	            	    if(rhs.charAt(i)==' ')
  	            	      sp_pos=i;
  	            	  }
  	            	  rhs=rhs.substring(sp_pos+1,rhs.length())
  	            	  //println(rhs)
  	            	}
  	            	//print("\nAssign('"+m_name+dlmtr+stmt_id+"','"+lhs+"','"+rhs+"').")
  	            	datalog.append("\nAssign('"+m_name+dlmtr+stmt_id+"','"+object_name+"','"+lhs+"','"+rhs+"').")
  	            }
  	            else
  	            {
  	            	//print("\nAssign('"+m_name+dlmtr+stmt_id+"','"+lhs+"','"+rhs+"').")
  	            	datalog.append("\nAssign('"+m_name+dlmtr+stmt_id+"','"+object_name+"','"+lhs+"','"+rhs+"').")
  	            }
  	            //println(m_name+stmt_id+"  "+arg_count)
  	          }
  	          
  	          //tracing special invoke related to object invokation
  	          if(statement_type.equals("specialinvoke") && !arg_name.equals("void") )
  	          {
  	            if(lhs_name.length()!=0)
  	            {
  	            	////print("@@@"+arg_count)
  	            	function_name="_constructor"         //overwrite <init()>
  	            	//-------- unification or not
  	            	datalog.append("\nSpecialInvoke('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+arg_count+"','"+lhs_name+"').")
  	            	invoke_obj()
  	            	argument()
  	            }
  	            else
  	            {
  	              ////print("@@@"+arg_count)
  	            	function_name="_constructor"         //overwrite <init()>
  	            	//-------- unification or not
  	            	datalog.append("\nSpecialInvoke('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+arg_count+"','"+temp_str+temp_cout+"').")
  	            	temp_cout=temp_cout+1 
  	            	invoke_obj()
  	            	argument()
  	            }
  	            //println(m_name+stmt_id+"  "+arg_count)
  	          }
  	          
  	          else if(statement_type.equals("specialinvoke") && arg_name.equals("void") )
  	          {
  	            if(lhs_name.length()!=0)
  	            {
  	            	////print("@@@"+arg_count)
  	            	function_name="_constructor"         //overwrite <init()>
  	            	//-------- unification or not
  	            	datalog.append("\nSpecialInvoke('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+arg_count+"','"+lhs_name+"').")
  	                invoke_obj()
  	            	argument()
  	            }
  	            else
  	            {
  	              ////print("@@@"+arg_count)
  	            	function_name="_constructor"         //overwrite <init()>
  	            	//-------- unification or not
  	            	datalog.append("\nSpecialInvoke('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+arg_count+"','"+temp_str+temp_cout+"').")
  	                temp_cout=temp_cout+1 
  	            	invoke_obj()
  	                argument()
  	            }
  	            //println(m_name+stmt_id+"  "+arg_count)
  	          }
  	          
  	          //tracing assignment statements by static invoke due to auto-boxing
  	          
  	          if(statement_type.equals("staticinvoke") && function_name.equals("valueOf()") && lhs_name.length()>0)
  	          {
  	            var lhs:String=lhs_name
  	            if(lhs.contains("\"") || lhs.contains("'") || isNum(lhs_name_p))
  	            lhs="_constant"
  	              
  	            var rhs:String=arg_name
  	            if(rhs.contains("\"") || rhs.contains("'") || isNum(lhs_name_p))
  	            rhs="_constant"
  	            
  	            //print("\nAssign('"+m_name+dlmtr+stmt_id+"','"+lhs+"','"+rhs+"').")
  	            datalog.append("\nAssign('"+m_name+dlmtr+stmt_id+"','"+object_name+"','"+lhs+"','"+rhs+"').")
  	          }
  	          
  	          if(statement_type.equals("staticinvoke") && !function_name.equals("valueOf()") && lhs_name.length()>0)
  	          {
  	            //print("\nStaticInvoke('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+"0"+"','"+lhs_name+"').")
  	            datalog.append("\nStaticInvoke('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+"0"+"','"+lhs_name+"').")
  	            invoke_obj()
  	            argument()  
  	            //println(m_name+stmt_id)
  	          }
  	          
  	          if(statement_type.equals("currentobjectinvoke"))
  	          {
  	            //print("\nInvoke('"+m_name+dlmtr+stmt_id+"','"+"_constructor"+"','"+"0"+"','"+temp_str+temp_cout+"').")
  	            datalog.append("\nInvoke('"+m_name+dlmtr+stmt_id+"','"+"_constructor"+"','"+"0"+"','"+lhs_name+"').")
  	            temp_cout=temp_cout+1
  	            //println(m_name+stmt_id+"  "+arg_count)
  	          }
  	          
  	          //trace method call
  	          if(lhs_name.length()==0 && statement_type.equals("virtualinvoke"))
  	          {
  	        	 ////print("##"+stmt_id)
  	             //print("\nVirtualInvoke('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+arg_count+"','"+temp_str+temp_cout+"').")
  	             datalog.append("\nVirtualInvoke('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+arg_count+"','"+temp_str+temp_cout+"').")
  	             temp_cout=temp_cout+1 	             
  	             invoke_obj()
  	             argument()   
  	          }
  	          //trace virtual invoke call with lhs name exists
  	          if(lhs_name.length()>0 && statement_type.equals("virtualinvoke"))
  	          {
  	        	 ////print("##"+stmt_id)
  	             //print("\nVirtualInvoke('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+arg_count+"','"+temp_str+temp_cout+"').")
  	             datalog.append("\nVirtualInvoke('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+arg_count+"','"+lhs_name+"').")      
  	             invoke_obj()
  	             argument()
  	             //problem for complicated string
  	             
  	          }
  	          
  	           if(lhs_name.length()>0 && statement_type.equals("interfaceinvoke"))
  	           {
  	        	 //print("\nInterfaceInvoke('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+arg_count+"','"+lhs_name+"').")
  	             datalog.append("\nInterfaceInvoke('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+arg_count+"','"+lhs_name+"').")
  	             invoke_obj()
  	             argument()
  	           }
  	           
  	          //tracing return statement
  	          if(statement_type.equals("returnstatement"))
  	          {
  	            var return_var:String=stmt \\ "return_var" \ "@name" text;
  	            if(return_var.contains("\"") || return_var.contains("'"))
  	              return_var="_constant"
  	            //print("\nReturn('"+m_name+dlmtr+stmt_id+"','"+return_var+"').")
  	            datalog.append("\nReturn('"+m_name+dlmtr+stmt_id+"','"+return_var+"').")
  	          }
  	          //print(stmt_id+"::"+stmt_line+"\n")
                  //println("-------------")
                  //
                  //trace for in and out variables
             if(statement_type.equals("staticinvoke") || statement_type.equals("invoke")
                || statement_type.equals("interfaceinvoke") || statement_type.equals("virtualinvoke")
                || statement_type.equals("specialinvoke"))
                    {
                      in_live_var.foreach(in=>
                        {
                          datalog.append("\nInVar('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+in._2+"').")
                        })
                        
                      out_live_var.foreach(out=>
                        {
                          datalog.append("\nOutVar('"+m_name+dlmtr+stmt_id+"','"+function_name+"','"+out._2+"').")
                        })  
                    }
  	      })
  	      //statement end
  	      
  	      
  	       datalog.append("\n\n")
  	    })//method
  	  })//class
  	  
  	  datalog.append("\n\n//End of datalog facts")
  	  datalog.append("\n// datalog query to run:")
  	  datalog.close()
  	  println("-----write complete-----")
	 })
	 
  }
}