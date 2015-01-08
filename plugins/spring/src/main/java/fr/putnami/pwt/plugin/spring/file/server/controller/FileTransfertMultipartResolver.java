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

	private static final ThreadLocal<FileTransfertProgressListener> TL_LISTENERS =
		new ThreadLocal<FileTransfertProgressListener>();

	@Value("${filetransfertcontroller.maxUploadSize}")
	private long maxUploadSize;

	@Autowired
	private FileTransfertController fileTransfertController;

	@PostConstruct
	public void postConstruct() {
		this.setMaxUploadSize(this.maxUploadSize);
	}

	@Override
	protected FileUpload newFileUpload(FileItemFactory fileItemFactory) {
		FileUpload fileUpload = super.newFileUpload(fileItemFactory);
		fileUpload.setProgressListener(TL_LISTENERS.get());
		TL_LISTENERS.remove();
		return fileUpload;
	}


	@Override
	public MultipartHttpServletRequest resolveMultipart(HttpServletRequest request) {
		FileTransfertProgressListener progress = new FileTransfertProgressListener();
		TL_LISTENERS.set(progress);
		this.fileTransfertController.startUpload(this.getUploadId(request), progress);
		return super.resolveMultipart(request);
	}

	@Override
	public void cleanupMultipart(MultipartHttpServletRequest request) {
		this.fileTransfertController.completeUpload(this.getUploadId(request));
		super.cleanupMultipart(request);
	}

	private String getUploadId(HttpServletRequest request) {
		return request.getRequestURI().replace("/file/upload/", "");
	}
}
