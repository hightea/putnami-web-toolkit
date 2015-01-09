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

import com.google.common.collect.Maps;

import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.commons.CommonsMultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import fr.putnami.pwt.core.widget.shared.domain.FileDto;
import fr.putnami.pwt.core.widget.shared.domain.UploadStatus;
import fr.putnami.pwt.plugin.spring.file.server.support.FileTransfertStore;

@Service
@Controller
public class FileTransfertController {

	@Autowired
	private FileTransfertStore store;

	private Map<String, FileTransfertProgressListener> progresses = Maps.newConcurrentMap();

	@RequestMapping(value = "/file/upload/{uploadId}", method = RequestMethod.POST)
	@ResponseBody
	public FileDto upload(@PathVariable String uploadId,
		@RequestParam("data") CommonsMultipartFile multipart, HttpServletRequest request,
		HttpServletResponse response) {
		OutputStream out = null;
		InputStream in = null;
		try {
			out = this.store.write(uploadId, multipart.getOriginalFilename(), multipart.getContentType());
			in = multipart.getInputStream();
			IOUtils.copy(in, out);
			return this.store.getFileBean(uploadId);
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage(), e);
		} finally {
			IOUtils.closeQuietly(in);
			IOUtils.closeQuietly(out);
		}
	}

	@RequestMapping(value = "/file/status/{uploadId}", method = RequestMethod.GET)
	@ResponseBody
	public UploadStatus getUploadStatus(@PathVariable String uploadId) {
		FileTransfertProgressListener progress = this.progresses.get(uploadId);
		if (progress != null) {
			return new UploadStatus(uploadId, progress.getBytesRead(), progress.getContentLength());
		}
		return null;
	}

	@RequestMapping(value = "/file/download/{fileId}", method = RequestMethod.GET)
	public void downloadFile(@PathVariable String fileId, HttpServletRequest request,
		HttpServletResponse response) {
		try {
			FileDto fileBean = this.store.getFileBean(fileId);
			if (fileBean == null) {
				throw new RuntimeException("Aucun fichier trouver " + fileId);
			}
			InputStream is = this.store.read(fileId);
			response.setContentType(fileBean.getMime());
			response.setHeader("Content-Disposition", "attachment; filename=\""
				+ fileBean.getName() + "\"");
			response.setContentLength((int) fileBean.getContentLength());
			IOUtils.copy(is, response.getOutputStream());
			response.flushBuffer();
		} catch (IOException ex) {
			throw new RuntimeException("IOError writing file to output stream", ex);
		}
	}

	public void startUpload(String uploadId, FileTransfertProgressListener progress) {
		this.progresses.put(uploadId, progress);
	}

	public void completeUpload(String uploadId) {
		this.progresses.remove(uploadId);
	}

}
