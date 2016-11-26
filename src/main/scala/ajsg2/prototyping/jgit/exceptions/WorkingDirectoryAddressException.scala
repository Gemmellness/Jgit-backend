package ajsg2.prototyping.jgit.exceptions

case class WorkingDirectoryAddressException(message: String = "", cause: Throwable = null)
  extends Exception(message, cause)