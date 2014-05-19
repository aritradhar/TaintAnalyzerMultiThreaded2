package selfExecutingCmdLine

import scala.collection.JavaConversions._
import org.deri.iris.facts.Facts
import java.io.FileWriter
import org.deri.iris.optimisations.rulefilter._
import org.deri.iris.api.basics.IQuery

class Datalog_query
{            
   val r = new org.deri.iris.compiler.Parser
   val q = new org.deri.iris.compiler.Parser
   var kb:org.deri.iris.api.IKnowledgeBase = _
   
   def loadRules(s:String) = 
	{
		r.parse(s)
		val conf = org.deri.iris.KnowledgeBaseFactory.getDefaultConfiguration
		kb = org.deri.iris.KnowledgeBaseFactory.createKnowledgeBase(r.getFacts, r.getRules, conf)
	}

	def query(s:String,path:String) : String= 
	  try 
	{		
	    var output:String=""
	    //println("Query String : "+s)
	      println("<thread executing> for file : "+path.substring(0, path.indexOf("_datalog_output.txt")).concat("_datalog.dl"))
	    q.parse(s)
		val answer = kb.execute(q.getQueries.apply(0))
		//println(answer.toString.replace("), (", "),\n ("))
		//println("Total rows: "+answer.size)
		output=output.concat(answer.toString.replace("), (", "),\n ("))
		output=output.concat("\n").concat("Total rows: "+answer.size+"\n")
		
		return output
	} 
	
	
	def writequery(s:String) :String=
	try 
	{		 
	    q.parse(s)
		val answer = kb.execute(q.getQueries.apply(0))
		var out_s:String=""
		out_s=out_s.concat(answer.toString.replace("), (", "),\n (")).concat("\nTotal rows: "+answer.size)
		return out_s
	}

	def loadFile(file:String) = 
	{
		val s = trap.file.Util.readTextFileToString(file)
		println("String is: "+s)
		println("If string is empty, please check filename")
		loadRules(s)
	}
}