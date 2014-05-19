package selfExecutingCmdLine

import scala.collection.JavaConversions._
import org.deri.iris.facts.Facts
import java.io.FileWriter
import org.deri.iris.optimisations.rulefilter._
import org.deri.iris.api.basics.IQuery
import scala.actors.Actor


class Runner
{
  def main(arg:Array[String]) :Unit =
  {
    var fact:String=arg(0)
    var query:String=arg(1)
    var path:String=arg(2) 
	var ex=new Execute_concurrent(fact,query,path)
	ex start
  }
}

class Execute_concurrent(fact:String,query:String,outpath:String) extends Actor
{
  def act=
  {
    var da:Datalog_func=new Datalog_func()
    da.loadRules(fact)
    var ans:String=da.query(query,outpath)
    var fwrite:java.io.FileWriter=new FileWriter(outpath,true)
    fwrite.append(ans)
    fwrite.close()
  }
}

class Datalog_func
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

	def query(s:String, path:String) : String = 
	  try 
	{		
	    println("<actor execution> for file : "+path.substring(0, path.indexOf("_datalog_output.txt")).concat("_datalog.dl"))
	    q.parse(s)
		val answer = kb.execute(q.getQueries.apply(0))
		var ans:String=answer.toString.replace("), (", "),\n (").concat("\n"+"Total rows: "+answer.size+"\n")
		return ans
	} 
}