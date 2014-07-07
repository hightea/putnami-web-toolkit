package fr.putnami.pwt.core.security.shared.service;

import fr.putnami.pwt.core.security.shared.domain.SessionDto;
import fr.putnami.pwt.core.security.shared.domain.SignInRequest;
import fr.putnami.pwt.core.security.shared.exception.SignFailledException;

public interface SessionService {

	SessionDto getCurrentSession();

	SessionDto signIn(SignInRequest request) throws SignFailledException;

	SessionDto signOut();

}
