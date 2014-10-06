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

	private static ThreadLocal<FileTransfertProgressListener> tlUploadId =
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
	public void cleanupMultipart(MultipartHttpServletRequest request) {
		this.fileTransfertController.completeUpload(this.getUploadId(request));
		FileTransfertMultipartResolver.tlUploadId.remove();
		super.cleanupMultipart(request);
	}

	@Override
	protected FileUpload newFileUpload(FileItemFactory fileItemFactory) {
		FileUpload fileUpload = super.newFileUpload(fileItemFactory);
		fileUpload.setProgressListener(FileTransfertMultipartResolver.tlUploadId.get());
		return fileUpload;
	}

	@Override
	public MultipartHttpServletRequest resolveMultipart(HttpServletRequest request) {
		FileTransfertProgressListener progress = new FileTransfertProgressListener();
		FileTransfertMultipartResolver.tlUploadId.set(progress);
		this.fileTransfertController.startUpload(this.getUploadId(request),
				FileTransfertMultipartResolver.tlUploadId.get());
		return super.resolveMultipart(request);
	}

	private String getUploadId(HttpServletRequest request) {
		return request.getPathInfo().replace("/file/upload/", "");
	}
}
