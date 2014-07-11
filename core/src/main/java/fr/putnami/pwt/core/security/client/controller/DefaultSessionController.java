package fr.putnami.pwt.core.security.client.controller;

import com.google.gwt.core.shared.GWT;

import fr.putnami.pwt.core.security.shared.domain.SessionDto;
import fr.putnami.pwt.core.security.shared.domain.SigninDto;
import fr.putnami.pwt.core.security.shared.service.SessionService;
import fr.putnami.pwt.core.service.client.ServiceProxy;
import fr.putnami.pwt.core.service.client.annotation.AsyncHandler;

public class DefaultSessionController extends SessionController {

	interface SessionServiceRemote extends ServiceProxy<DefaultSessionController, SessionService>, SessionService {
	}

	private final SessionServiceRemote service = (SessionServiceRemote) GWT.create(SessionServiceRemote.class);

	public DefaultSessionController() {
		service.bindService(this);
	}

	@Override
	public void loadSession() {
		service.getCurrentSession();
	}

	@Override
	public void signIn(SigninDto request) {
		service.signIn(request);
	}

	@Override
	public void signOut() {
		service.signOut();
	}

	@AsyncHandler
	void onGetCurrentSession(SessionDto session) {
		setSession(session);
		fireSignIn();
	}


	@AsyncHandler
	void onSignIn(SessionDto session) {
		setSession(session);
		fireSignIn();
	}

	@AsyncHandler(method = "signIn")
	void onSignIn(Throwable e) {
		fireSignFailled();
	}

	@AsyncHandler
	void onSignOut(SessionDto session) {
		setSession(session);
		fireSignOut();
	}

}
