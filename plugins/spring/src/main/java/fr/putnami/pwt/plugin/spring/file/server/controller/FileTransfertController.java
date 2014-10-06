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
	public FileDto upload(
			@PathVariable String uploadId,
			@RequestParam("data") CommonsMultipartFile multipart,
			HttpServletRequest request, HttpServletResponse response) {
		InputStream in = null;
		OutputStream out = null;
		try {
			in = multipart.getInputStream();
			out = store.write(uploadId, multipart.getOriginalFilename(), multipart.getContentType());
			IOUtils.copy(in, out);
			return store.getFileBean(uploadId);
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
		FileTransfertProgressListener progress = progresses.get(uploadId);
		if (progress != null) {
			return new UploadStatus(uploadId, progress.getBytesRead(), progress.getContentLength());
		}
		return null;
	}

	@RequestMapping(value = "/file/download/{fileId}", method = RequestMethod.GET)
	public void downloadFile(
			@PathVariable String fileId,
			HttpServletRequest request,
			HttpServletResponse response) {
		try {
			FileDto fileBean = store.getFileBean(fileId);
			if (fileBean == null) {
				throw new RuntimeException("Aucun fichier trouver " + fileId);
			}
			InputStream is = store.read(fileId);
			response.setContentType(fileBean.getMime());
			response.setHeader("Content-Disposition", "attachment; filename=\"" + fileBean.getName() + "\"");
			response.setContentLength((int) fileBean.getContentLength());
			IOUtils.copy(is, response.getOutputStream());
			response.flushBuffer();
		} catch (IOException ex) {
			throw new RuntimeException("IOError writing file to output stream", ex);
		}

	}


	public void startUpload(String uploadId, FileTransfertProgressListener progress) {
		progresses.put(uploadId, progress);
	}

	public void completeUpload(String uploadId) {
		progresses.remove(uploadId);
	}

}
