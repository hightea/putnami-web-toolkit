package fr.putnami.pwt.core.widget.shared.domain;

import java.io.Serializable;

public class UploadStatus implements Serializable {

	private Long bytesRead;
	private Long contentLength;
	private String key;

	private String message = null;

	public UploadStatus() {

	}

	public UploadStatus(final String uploadId, final String message) {
		super();
		this.key = uploadId;
		this.message = message;
		this.bytesRead = 0L;
		this.contentLength = 0L;
	}

	public UploadStatus(final String uploadId, final Long part, final Long total) {
		super();
		this.key = uploadId;
		this.bytesRead = part;
		this.contentLength = total;
	}

	public String getDisplaySpeed() {
		return "100Mb/sec";
	}

	public String getMessage() {
		return message;
	}

	public String getName() {
		return key;
	}

	public Long getBytesRead() {
		return bytesRead;
	}

	public Long getContentLength() {
		return contentLength;
	}

	public String getKey() {
		return key;
	}

	public Integer getPercentage() {
		if (contentLength.equals(0L)) {
			return 0;
		}
		else {
			double r = bytesRead * 100D / contentLength;
			return Long.valueOf(Math.round(r)).intValue();
		}
	}

	public Boolean isFinished() {
		return getPercentage().equals(100);
	}

	public void setBytesRead(Long bytesRead) {
		this.bytesRead = bytesRead;
	}

	public void setContentLength(Long contentLength) {
		this.contentLength = contentLength;
	}

	public void setKey(String key) {
		this.key = key;
	}

	public void setMessage(String message) {
		this.message = message;
	}
}
