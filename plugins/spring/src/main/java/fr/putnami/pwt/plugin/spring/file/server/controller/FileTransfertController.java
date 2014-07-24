package fr.putnami.pwt.plugin.spring.file.server.controller;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.commons.CommonsMultipartFile;

import com.google.common.collect.Maps;
import com.google.common.io.Files;

import fr.putnami.pwt.core.widget.shared.domain.FileDto;
import fr.putnami.pwt.core.widget.shared.domain.UploadStatus;
import fr.putnami.pwt.core.widget.shared.service.UploadService;
import fr.putnami.pwt.plugin.spring.file.server.controller.MultipartResolver.MultipartProgressListener;
import fr.putnami.pwt.plugin.spring.rpc.server.util.RequestThreadLocalUtils;

@Service
@Controller
public class FileTransfertController implements UploadService {

	@Value("${filetransfertcontroller.tempdir}")
	private File tempdir;

	private Map<String, FileDto> files = Maps.newHashMap();

	@RequestMapping(value = "/uploadfile", method = RequestMethod.POST)
	@ResponseBody
	public FileDto upload(
			@RequestParam("uploadPostId") String uploadPostId,
			@RequestParam("data") CommonsMultipartFile file,
			HttpServletRequest request, HttpServletResponse response) {
		InputStream stream = null;
		try {
			stream = file.getInputStream();
			String token = UUID.randomUUID().toString();
			File target = new File(tempdir, token);
			Files.createParentDirs(target);
			IOUtils.copy(stream, new FileOutputStream(target));
			return getFileBean(token, file.getOriginalFilename(), file.getContentType());
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage(), e);
		} finally {
			IOUtils.closeQuietly(stream);
		}
	}

	@RequestMapping(value = "/download/{fileId}", method = RequestMethod.GET)
	public void downloadFile(
			@PathVariable String fileId,
			HttpServletRequest request,
			HttpServletResponse response) {
		try {
			FileDto fileBean = files.get(fileId);
			if (fileBean == null) {
				throw new RuntimeException("Aucun fichier trouver " + fileId);
			}
			File file = new File(tempdir, fileId);
			if (!file.isFile()) {
				throw new RuntimeException("Aucun fichier trouver " + fileId);
			}
			InputStream is = new FileInputStream(file);
			response.setContentType(fileBean.getMime());
			response.setHeader("Content-Disposition", "attachment; filename=\"" + fileBean.getName() + "\"");
			response.setContentLength((int) fileBean.getContentLength());
			IOUtils.copy(is, response.getOutputStream());
			response.flushBuffer();
		} catch (IOException ex) {
			throw new RuntimeException("IOError writing file to output stream", ex);
		}

	}

	@Override
	public UploadStatus getUploadStatus(Long uploadPostId) {
		HttpSession session = RequestThreadLocalUtils.getSession();
		MultipartProgressListener progressListener = (MultipartProgressListener) session.getAttribute("ProgressListener_" + uploadPostId);
		if (progressListener != null) {
			return progressListener.getUploadStatus();
		}
		return null;
	}

	public FileDto getFileBean(String token, String fileName, String contentType) {

		File file = new File(tempdir, token);

		FileDto fileDto = new FileDto();
		fileDto.setToken(token);
		fileDto.setName(fileName);
		fileDto.setMime(contentType);
		fileDto.setContentLength(file.length());
		fileDto.setExtension(FilenameUtils.getExtension(fileName));

		files.put(token, fileDto);

		return fileDto;
	}

}
