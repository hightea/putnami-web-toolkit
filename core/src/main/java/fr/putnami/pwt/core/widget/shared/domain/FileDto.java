package fr.putnami.pwt.core.widget.shared.domain;

import java.io.Serializable;

public class FileDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -8731024091131752897L;
	private String name;
	private String extension;
	private String mime;
	private String token;
	private long contentLength;

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getExtension() {
		return this.extension;
	}

	public void setExtension(String extension) {
		this.extension = extension;
	}

	public String getMime() {
		return this.mime;
	}

	public void setMime(String mime) {
		this.mime = mime;
	}

	public String getToken() {
		return this.token;
	}

	public void setToken(String token) {
		this.token = token;
	}

	public long getContentLength() {
		return this.contentLength;
	}

	public void setContentLength(long contentLength) {
		this.contentLength = contentLength;
	}

}
