package fr.putnami.pwt.core.mvp.client.exception;

public class ApplicationUnreachableException extends RuntimeException {

	private static final long serialVersionUID = -5554909211111117607L;

	public static final String HTTP_DOWNLOAD_FAILURE_EXCEPTION = "AsyncFragmentLoader$HttpDownloadFailure";

	public ApplicationUnreachableException() {
	}

	public ApplicationUnreachableException(Throwable cause) {
		super(cause);
	}

}
