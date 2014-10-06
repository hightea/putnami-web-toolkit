package fr.putnami.pwt.plugin.spring.file.server.controller;

import org.apache.commons.fileupload.ProgressListener;

import java.io.Serializable;

public class FileTransfertProgressListener implements ProgressListener, Serializable {

	private static final long serialVersionUID = -4451939328529354450L;

	private long bytesRead = 0;
	private long contentLength = 0;

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


	@Override
	public void update(long bytesRead, long contentLength, int pItems) {
		this.bytesRead = bytesRead;
		this.contentLength = contentLength;
	}
}