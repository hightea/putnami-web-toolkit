package fr.putnami.pwt.plugin.spring.file.server.controller;

import java.io.Serializable;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.fileupload.FileItemFactory;
import org.apache.commons.fileupload.FileUpload;
import org.apache.commons.fileupload.ProgressListener;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.web.multipart.MultipartHttpServletRequest;
import org.springframework.web.multipart.commons.CommonsMultipartResolver;

import fr.putnami.pwt.core.widget.shared.domain.UploadStatus;

public class MultipartResolver extends CommonsMultipartResolver {

	public static class MultipartProgressListener implements ProgressListener, Serializable {

		private static final long serialVersionUID = -4451939328529354450L;

		private String uploadPostId;
		private long bytesRead = 0;
		private long contentLength = 0;
		private boolean multipartFinished = false;

		public MultipartProgressListener(HttpServletRequest request) {
			uploadPostId = request.getParameter("uploadPostId");
			request.getSession().setAttribute("ProgressListener_" + uploadPostId, this);
		}

		public void setMultipartFinished() {
			this.multipartFinished = true;
		}

		public boolean isFinished() {
			return multipartFinished;
		}

		public int getPercentDone() {
			if (contentLength == -1) {
				// ensure we never reach 100% but show progress
				return (int) Math.abs(bytesRead * 100.0 / (bytesRead + 10000));
			}
			return (int) Math.abs(bytesRead * 100.0 / contentLength);
		}

		public UploadStatus getUploadStatus() {
			return new UploadStatus(uploadPostId, bytesRead, contentLength);
		}

		@Override
		public void update(long bytesRead, long contentLength, int pItems) {
			this.bytesRead = bytesRead;
			this.contentLength = contentLength;
		}
	}

	protected final Log logger = LogFactory.getLog(this.getClass());
	private static ThreadLocal<MultipartProgressListener> progressListener = new ThreadLocal<MultipartProgressListener>();

	@Override
	public void cleanupMultipart(MultipartHttpServletRequest request) {
		progressListener.get().setMultipartFinished();
		super.cleanupMultipart(request);
	}

	@Override
	protected FileUpload newFileUpload(FileItemFactory fileItemFactory) {
		FileUpload fileUpload = super.newFileUpload(fileItemFactory);
		fileUpload.setProgressListener(progressListener.get());
		return fileUpload;
	}

	@Override
	public MultipartHttpServletRequest resolveMultipart(HttpServletRequest request) {
		progressListener.set(new MultipartProgressListener(request));
		return super.resolveMultipart(request);
	}
}
