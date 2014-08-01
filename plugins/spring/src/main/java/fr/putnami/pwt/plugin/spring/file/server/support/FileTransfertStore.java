package fr.putnami.pwt.plugin.spring.file.server.support;

import java.io.InputStream;
import java.io.OutputStream;

import fr.putnami.pwt.core.widget.shared.domain.FileDto;

public interface FileTransfertStore {

	InputStream read(String fileId);

	OutputStream write(String fileId, String originalFilename, String contentType);

	FileDto getFileBean(String fileId);

}
