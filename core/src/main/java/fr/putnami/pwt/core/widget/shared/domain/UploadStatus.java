/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.shared.domain;

import java.io.Serializable;

public class UploadStatus implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 2110831890820359599L;
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
		return this.uploadId;
	}

	public void setUploadId(String uploadId) {
		this.uploadId = uploadId;
	}

	public long getBytesRead() {
		return this.bytesRead;
	}

	public void setBytesRead(long bytesRead) {
		this.bytesRead = bytesRead;
	}

	public long getContentLength() {
		return this.contentLength;
	}

	public void setContentLength(long contentLength) {
		this.contentLength = contentLength;
	}

}
