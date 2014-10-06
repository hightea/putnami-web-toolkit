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
package fr.putnami.pwt.plugin.spring.file.server.support;

import com.google.common.collect.Maps;
import com.google.common.io.Files;

import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import fr.putnami.pwt.core.widget.shared.domain.FileDto;

@Component
public class DefaultFileTransfertStore implements FileTransfertStore {

	@Value("${filetransfertcontroller.tempdir}")
	private File tempdir;

	private final Map<String, FileDto> files = Maps.newConcurrentMap();

	@PostConstruct
	public void postConstruct() {
		this.clearFiles(this.tempdir);
	}

	@PreDestroy
	public void destroy() {
		this.clearFiles(this.tempdir);
	}

	@Override
	public InputStream read(String fileId) {
		try {
			return new FileInputStream(new File(this.tempdir, fileId));
		} catch (FileNotFoundException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public OutputStream write(String fileId, String fileName, String contentType) {
		try {
			FileDto fileDto = new FileDto();
			fileDto.setToken(fileId);
			fileDto.setName(fileName);
			fileDto.setMime(contentType);
			fileDto.setExtension(FilenameUtils.getExtension(fileName));

			this.files.put(fileId, fileDto);

			File target = new File(this.tempdir, fileId);
			Files.createParentDirs(target);
			return new FileOutputStream(target);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public FileDto getFileBean(String fileId) {
		FileDto dto = this.files.get(fileId);
		if (dto == null) {
			return null;
		}
		dto.setContentLength(new File(this.tempdir, fileId).length());
		return dto;
	}

	private void clearFiles(File file) {
		if (file.isDirectory()) {
			for (File child : file.listFiles()) {
				this.clearFiles(child);
			}
			if (file.listFiles().length == 0) {
				file.delete();
			}
		} else if (file.isFile()) {
			file.delete();
		}
	}

}
