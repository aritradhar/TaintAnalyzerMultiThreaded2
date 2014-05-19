//author: Aritra Dhar
//Intern
//Accenture Technology Labs
//version 1.4

package selfExecutingCmdLine

import scala.collection.JavaConversions._
import scala.io.Source
import org.deri.iris.facts.Facts
import java.io.FileWriter
import scala.util.control.Breaks

class DatalogPerser
{
   var fact_path:String=""
   var rules_path:String=""
   var rules:String=""
   var query_path:String=""
   var qry:String=""
   var flag:Int=0
   var flag_global:Int=0
   var static_fld_fact:String=""
     
  // read facts by method
  //file to load ::  loc+"datalog\\"+filename_datalog
  
  
  var factString:String=""
  var method_count:Int=0
  var MAX:Int=5000           //for large project it is subjected to be changed

  
  def method_detector(filepath:String) : Array[String] =
  {
     	var fact_array:Array[String]=new Array[String](MAX)
     	//trace static class fields
    // rules_path=""
     //fact_path=""
        
       for(temp_r <- Source.fromFile("rule_path.txt").getLines())
       {
	   rules_path=rules_path.concat(temp_r)
       }
    //rules
  
  		for(temp_r1 <- Source.fromFile(rules_path).getLines())
  		{
  			rules=rules.concat(temp_r1).concat("\n")
  		}
  		/*
       for(temp_r <- Source.fromFile("fact_path.txt").getLines())
   		{
	   		fact_path=fact_path.concat(temp_r)
   		}
  		 * 
  		 */
  		for(temp_s <- Source.fromFile(filepath).getLines())
  		{
  			if(temp_s.equals("//method name:static field"))
  			{
  				flag=1
  				flag_global=1
  			}
  			if(temp_s.contains("//method name : "))
  				flag_global=0
  			if(flag_global==1)
  				static_fld_fact=static_fld_fact.concat(temp_s).concat("\n")
	 
  		}
    
  		for(temp_s <- Source.fromFile(filepath).getLines())
  		{
  			if(temp_s.contains("//method name"))
  				method_count=method_count+1
  			factString=factString.concat(temp_s).concat("\n")
  		}
  		var mth_point:Array[Int]=new Array[Int](method_count+1)
	
  		var i:Int=0
  		var j:Int=0
  		for(temp_s <- Source.fromFile(filepath).getLines())
  		{
	  
  			if(temp_s.contains("//method name"))
  			{
  				mth_point(j)=i
  				//println(temp_s+"       @ line"+i)
  				//println(i)
  				j=j+1
  			}
  			i=i+1;
  		}
  		mth_point(method_count)=i	
	
  		for(i <-0 to method_count-1)
  		{
  			fact_array(i)=""
  		}
	
  		i=0
  		j=0
  		var i1:Int=0
	
  		for(i<-0 to method_count-1)
  		{
  			if(flag==1 && i>0)
  			{
  				fact_array(i)=fact_array(i).concat(static_fld_fact)
  			}
  			fact_array(i)=fact_array(i).concat(rules)
  		}
	
  		for(temp_s <- Source.fromFile(filepath).getLines())
  		{	  
  			fact_array(i)=fact_array(i).concat(temp_s).concat("\n")
  			j=j+1
  			i1=i+1
  			if(mth_point(i1)==j)
  			{
  				i1=i1+1
  				i=i+1
  			}
  		}
  		return fact_array
  	}
  
}