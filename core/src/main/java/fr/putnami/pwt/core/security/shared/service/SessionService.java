package fr.putnami.pwt.core.security.shared.service;

import fr.putnami.pwt.core.security.shared.domain.SessionDto;
import fr.putnami.pwt.core.security.shared.domain.SigninDto;
import fr.putnami.pwt.core.security.shared.exception.BadAuthenticationException;

public interface SessionService {

	SessionDto getCurrentSession();

	SessionDto signIn(SigninDto request) throws BadAuthenticationException;

	SessionDto signOut();

}
