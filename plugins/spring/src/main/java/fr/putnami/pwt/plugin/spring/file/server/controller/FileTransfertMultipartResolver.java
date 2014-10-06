package fr.putnami.pwt.plugin.spring.file.server.controller;

import org.apache.commons.fileupload.FileItemFactory;
import org.apache.commons.fileupload.FileUpload;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.web.multipart.MultipartHttpServletRequest;
import org.springframework.web.multipart.commons.CommonsMultipartResolver;

import javax.servlet.http.HttpServletRequest;

import fr.putnami.pwt.core.inject.client.annotation.PostConstruct;

@Controller
public class FileTransfertMultipartResolver extends CommonsMultipartResolver {

	private static ThreadLocal<FileTransfertProgressListener> tlUploadId = new ThreadLocal<FileTransfertProgressListener>();

	@Value("${filetransfertcontroller.maxUploadSize}")
	private long maxUploadSize;

	@Autowired
	private FileTransfertController fileTransfertController;

	@PostConstruct
	public void postConstruct() {
		setMaxUploadSize(maxUploadSize);
	}

	@Override
	public void cleanupMultipart(MultipartHttpServletRequest request) {
		fileTransfertController.completeUpload(getUploadId(request));
		tlUploadId.remove();
		super.cleanupMultipart(request);
	}

	@Override
	protected FileUpload newFileUpload(FileItemFactory fileItemFactory) {
		FileUpload fileUpload = super.newFileUpload(fileItemFactory);
		fileUpload.setProgressListener(tlUploadId.get());
		return fileUpload;
	}

	@Override
	public MultipartHttpServletRequest resolveMultipart(HttpServletRequest request) {
		FileTransfertProgressListener progress = new FileTransfertProgressListener();
		tlUploadId.set(progress);
		fileTransfertController.startUpload(getUploadId(request), tlUploadId.get());
		return super.resolveMultipart(request);
	}

	private String getUploadId(HttpServletRequest request) {
		return request.getPathInfo().replace("/file/upload/", "");
	}
}
