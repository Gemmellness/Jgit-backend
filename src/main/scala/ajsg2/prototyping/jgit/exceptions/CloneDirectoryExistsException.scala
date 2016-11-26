package ajsg2.prototyping.jgit.exceptions

case class CloneDirectoryExistsException(message: String = "", cause: Throwable = null)
  extends Exception(message, cause)