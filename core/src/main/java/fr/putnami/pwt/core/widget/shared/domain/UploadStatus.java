package fr.putnami.pwt.core.widget.shared.domain;

import java.io.Serializable;

public class UploadStatus implements Serializable {

	private String uploadId;
	private long bytesRead;
	private long contentLength;

	public UploadStatus() {

	}

	public UploadStatus(String uploadId, long bytesRead, long contentLength) {
		super();
		this.uploadId = uploadId;
		this.bytesRead = bytesRead;
		this.contentLength = contentLength;
	}

	public String getUploadId() {
		return uploadId;
	}

	public void setUploadId(String uploadId) {
		this.uploadId = uploadId;
	}

	public long getBytesRead() {
		return bytesRead;
	}

	public void setBytesRead(long bytesRead) {
		this.bytesRead = bytesRead;
	}

	public long getContentLength() {
		return contentLength;
	}

	public void setContentLength(long contentLength) {
		this.contentLength = contentLength;
	}

}
