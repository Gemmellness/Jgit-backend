package ajsg2.prototyping.jgit

import java.io.{BufferedWriter, File, FileWriter, IOException}
import java.lang.Iterable
import java.net.{MalformedURLException, URI, URISyntaxException, URL}
import java.util.Date

import ajsg2.prototyping.jgit.exceptions._
import org.eclipse.jgit.api.ListBranchCommand.ListMode
import org.eclipse.jgit.api.{CloneCommand, Git, Status}
import org.eclipse.jgit.lib.{Ref, Repository}
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.storage.file.FileRepositoryBuilder

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.io.json._
import scalax.collection.io.json.descriptor.NodeDescriptor
import scalax.collection.io.json.descriptor.predefined.Di


/**
	* Created by Adam on 26/11/2016.
	*/
object Backend {

	var git : Git = _
	var repository : Repository = _
	var workingDir : File = _
	var graph : Graph[Commit, DiEdge] = _

	// Json descriptors

	// Custom serializer
	/*final class CommitSerializer extends CustomSerializer[Commit] (fmts => (
		{ case JArray(JString(hash) :: JString(author) :: JString(branch) :: JString(date) :: JInt(dateVal) :: JInt(depth) :: lol :: Nil) =>
			Commit(hash, author, branch, date, dateVal.longValue, depth.intValue, lol.extract[List[String]])
		},
		{ case Commit(hash, author, branch, date, _, depth, _) =>
			JArray(JString(hash) :: JString(author) :: JString(branch) :: JString(date.toString) :: JString(depth.toString):: Nil)
		}))*/

	val commitDescriptor = new NodeDescriptor[Commit](typeId = "Commits") {
		def id(node: Any): String = node match {
			case Commit(hash, _, _, _, _, _, _) => hash
		}
	}

	val descriptor = new Descriptor[Commit](
		defaultNodeDescriptor = commitDescriptor,
		defaultEdgeDescriptor = Di.descriptor[Commit]()
	)

	def main(args: Array[String]): Unit = {
		try{
			setDirectory("C:\\Users\\Adam\\OneDrive\\Documents\\Project\\prototyping\\backend\\testingfolder\\test")
			//clone("https://github.com/Stochast1c/solarhud.git")
			loadRepository()
			buildCommitGraph()
			outputJson(generateJson())

			for (_ <- io.Source.stdin.getLines)
				println(detectChangedFiles())

		}catch {
			case e: Exception => System.err.println("Exception handled:")
				e.printStackTrace()
		}
	}

	/**
		* Set the working directory to the one specified in the argument.
		*
		* @param address The (absolute) address of the directory
		*/
	@throws[WorkingDirectoryAddressException]
	def setDirectory(address: String): Unit = {
		val dir: File = new File(address)

		if(!dir.isAbsolute)
			throw WorkingDirectoryAddressException("Working directory change failed: address is not absolute")
		else
			workingDir = dir
	}

	 /**
		* Builds the commit graph, built of Commits and directed edges
		*/
	def buildCommitGraph(): Unit = {
		val nodes = new mutable.HashMap[String, Commit]
		val edges = new mutable.ListBuffer[DiEdge[Commit]]

		// Generate nodes
		val commits: Iterable[RevCommit] = git.log().all().call()

		commits.asScala.foreach(commit => {
			// Build commit object
			val date : Date = new Date(commit.getCommitTime.toLong*1000)
			val parents : List[RevCommit] = commit.getParents.toList
			val parentsHashes: List[String] = parents.map(_.getName)
			val c = Commit(commit.getName, commit.getAuthorIdent.getName + ", " + commit.getAuthorIdent.getEmailAddress,
				"", date.toString, date.getTime, -1, parentsHashes)

			nodes += ((c.hash, c))
		})

		// Generate edges
		val commits2 = git.log().all().call()

		commits2.asScala.foreach(commit => {
			val parents = commit.getParents
			val default = Commit("error", "error", "", new Date(0L).toString, 0L, -1, List(""))

			parents.foreach( (p : RevCommit) => edges += nodes.getOrElse(p.getName, default) ~> nodes.getOrElse(
				commit.getName, default))

		})

		val lolTypeErrors = edges.toArray

		graph = Graph.from(nodes.toArray.map(_._2), lolTypeErrors)

		// Generate maximum depths
		val root = graph.nodes.toSet.filter(!_.hasPredecessors).head

		def maxDepth(node : Graph[Commit, DiEdge]#NodeT, depth : Int) : Unit = {
			if(node.value.depth < depth) {
				node.value.depth = depth
				node.diSuccessors.foreach(maxDepth(_, depth + 1))
			}
		}

		maxDepth(root, 0)

		// Label branches
		@tailrec
		def labelBranch(node : Graph[Commit, DiEdge]#NodeT, branch : String) : Unit = {
			val successors = node.diSuccessors
			val predecessors = node.diPredecessors

			if(successors.size <= 1) {
				// End of branch or non-branching commit - label and recurse on first parent
				node.value.branch = branch
				if(predecessors.nonEmpty)
					labelBranch(predecessors.filter(_.value.hash == node.value.parents.head).head, branch)
			}else {
				// Branch created here

				// First check to see if any child branches match the branch being currently assigned. If not we should definitely assign this branch to prevent infinite loops
				if(node.diSuccessors.count(_.value.branch == branch) == 0){
					node.value.branch = branch
					if (predecessors.nonEmpty)
						labelBranch(predecessors.filter(_.value.hash == node.value.parents.head).head, branch)
				}else{
					// Else, compare child commit times
					val oldestSibling = node.diSuccessors.minBy(_.value.dateVal)

					if(oldestSibling.value.branch == branch) {
						// This node belongs to the oldest child's branch.
						node.value.branch = branch
						if (predecessors.nonEmpty)
							labelBranch(predecessors.filter(_.value.hash == node.value.parents.head).head, branch)
					}
				}
			}
		}

		// Start at each branch
		git.branchList().setListMode(ListMode.ALL).call().asScala.foreach((r : Ref) => labelBranch(graph.nodes.toSet.filter(
			_.value.hash == r.getObjectId.getName).head, r.getName))

		// Assign a name to unnamed branches
		var candidateBranches = graph.nodes.filter((n : Graph[Commit, DiEdge]#NodeT) => n.value.branch == "" &&
			n.diSuccessors.count(_.value.branch == "") == 0)

		var i = 0
		while(candidateBranches.nonEmpty){
			candidateBranches.zipWithIndex.foreach{ t => labelBranch(t._1, "unnamedbranch" + t._2)}
			candidateBranches = graph.nodes.filter((n : Graph[Commit, DiEdge]#NodeT) => n.value.branch == "" &&
					n.diSuccessors.count(_.value.branch == "") == 0)

			println(candidateBranches.size)
			i += 1
		}
	}

	/**
	  *
	  * @return A String representing the current graph in JSON format
	  */
	def generateJson() : String = {
		graph.toJson(descriptor)
	}

	def outputJson(json: String): Unit = {
		val writer = new BufferedWriter(new FileWriter("data.json"))
		writer.write(json)
		writer.close()
	}

	/**
		* Load the repository in the current working directory
		*/
	def loadRepository(): Unit = {
		val builder = new FileRepositoryBuilder()
		val repo = builder.setGitDir(new File(workingDir.toString  + "\\.git"))
			.readEnvironment() // scan environment GIT_* variables
			.findGitDir() // scan up the file system tree
			.build()

		repository = repo
		git = new Git(repo)
	}

	/**
		* @param url The URL of the repository to clone
		*/
	@throws[IOException]
	@throws[CloneDirectoryExistsException]
	def clone(url: String): Unit = {
		try {
			val uri: URI = new URL(url).toURI

			val clone: CloneCommand = Git.cloneRepository
				.setURI(uri.toString)
				.setDirectory(workingDir)

			if (workingDir.exists)
				throw CloneDirectoryExistsException("The directory " + workingDir.toString + " already exists")
			else {
				git = clone.call()
				repository = git.getRepository
				git.close()
				println("Clone operation completed successfully")
			}
		} catch {
			case
				_: MalformedURLException  => throw new IOException("Clone failed: Malformed URL")
				_: URISyntaxException  => throw new IOException("Clone failed: Malformed URL")
		}

	}

	def detectChangedFiles() : List[String] = {
		var output: List[String] = List[String]()
		val status: Status = git.status().call()

		for(m <- status.getModified.iterator.asScala)
			output = ("Modified: " + m) :: output

		for(m <- status.getMissing.iterator.asScala)
			output = ("Missing: " + m) :: output

		for(u <- status.getUntracked.iterator.asScala)
			output = ("Untracked: " + u) :: output

		output
	}

	sealed trait GitGraph
	case class Commit(hash:String, author:String, var branch:String, date:String, dateVal: Long, var depth:Int,
					  parents:List[String]) extends GitGraph
}


