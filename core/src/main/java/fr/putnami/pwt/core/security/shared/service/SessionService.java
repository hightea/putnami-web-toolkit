package fr.putnami.pwt.core.security.shared.service;

import fr.putnami.pwt.core.security.shared.domain.SessionDto;
import fr.putnami.pwt.core.security.shared.domain.SigninDto;
import fr.putnami.pwt.core.security.shared.exception.SignFailledException;

public interface SessionService {

	SessionDto getCurrentSession();

	SessionDto signIn(SigninDto request) throws SignFailledException;

	SessionDto signOut();

}
