package fr.putnami.pwt.core.security.client.controller;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.shared.GwtEvent;
import com.google.web.bindery.event.shared.HandlerRegistration;

import fr.putnami.pwt.core.event.client.EventBus;
import fr.putnami.pwt.core.security.client.event.SignFailledEvent;
import fr.putnami.pwt.core.security.client.event.SignFailledEvent.HasSignFailledHandlers;
import fr.putnami.pwt.core.security.client.event.SignInEvent;
import fr.putnami.pwt.core.security.client.event.SignInEvent.Handler;
import fr.putnami.pwt.core.security.client.event.SignInEvent.HasSignInHandlers;
import fr.putnami.pwt.core.security.client.event.SignOutEvent;
import fr.putnami.pwt.core.security.client.event.SignOutEvent.HasSignOutHandlers;
import fr.putnami.pwt.core.security.shared.constant.SecurityConstants;
import fr.putnami.pwt.core.security.shared.domain.SessionDto;
import fr.putnami.pwt.core.security.shared.domain.SignInRequest;

public abstract class SessionController implements HasSignInHandlers, HasSignOutHandlers, HasSignFailledHandlers {

	private static SessionController instance;

	public static SessionController get() {
		if (instance == null) {
			instance = GWT.create(SessionController.class);
		}
		return instance;
	}

	private SessionDto session;

	protected SessionController() {

	}

	public boolean isAuthenticated() {
		return session != null
				&& !SecurityConstants.USER_ANONYMOUS.equals(session.getUsername());
	}

	public boolean hasRole(String role) {
		if (session != null) {
			String corrected = role;
			if (!corrected.startsWith(SecurityConstants.PREFFIX_ROLE)) {
				corrected = SecurityConstants.PREFFIX_ROLE + role;
			}
			corrected = corrected.toUpperCase();
			if (SecurityConstants.ROLE_ANONYMOUS.equals(corrected)) {
				return !isAuthenticated();
			}
			if (SecurityConstants.ROLE_AUTHENTICATED.equals(corrected)) {
				return isAuthenticated();
			}
			else {
				return session.getRoles().contains(corrected);
			}
		}
		return false;
	}

	protected void setSession(SessionDto session) {
		if (this.session == null || !this.session.equals(session)) {
			this.session = session;
		}
	}

	protected void fireSignIn() {
		fireEvent(new SignInEvent(session));
	}

	protected void fireSignFailled() {
		fireEvent(new SignFailledEvent(session));
	}

	protected void fireSignOut() {
		fireEvent(new SignOutEvent(session));
		fireSignIn();
	}

	@Override
	public HandlerRegistration addSignInHandler(Handler handler) {
		return EventBus.get().addHandlerToSource(SignInEvent.TYPE, this, handler);
	}

	@Override
	public HandlerRegistration addSignOutHandler(SignOutEvent.Handler handler) {
		return EventBus.get().addHandlerToSource(SignOutEvent.TYPE, this, handler);
	}

	@Override
	public HandlerRegistration addSignFailledHandler(SignFailledEvent.Handler handler) {
		return EventBus.get().addHandlerToSource(SignFailledEvent.TYPE, this, handler);
	}

	@Override
	public void fireEvent(GwtEvent<?> event) {
		EventBus.get().fireEventFromSource(event, this);
	}

	public abstract void signIn(SignInRequest sessionRequest);

	public abstract void signOut();

}
