package fr.putnami.pwt.core.widget.shared.service;

import fr.putnami.pwt.core.widget.shared.domain.UploadStatus;

public interface UploadService {
	UploadStatus getUploadStatus(Long uploadPostId);
}
