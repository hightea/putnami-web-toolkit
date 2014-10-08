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
package fr.putnami.pwt.core.security.client.controller;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.place.shared.Place;
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
import fr.putnami.pwt.core.security.shared.exception.SecurityException;

public abstract class SessionController implements HasSignInHandlers, HasSignOutHandlers, HasSignFailledHandlers {

	private static SessionController instance;

	private SessionDto session;

	protected SessionController() {
	}

	public SessionDto getCurrentSession() {
		return this.session;
	}

	public boolean isAuthenticated() {
		return this.session != null && !SecurityConstants.USER_ANONYMOUS.equals(this.session.getUsername());
	}

	public boolean hasRole(String role) {
		if (this.session != null) {
			String corrected = role;
			if (!corrected.startsWith(SecurityConstants.PREFFIX_ROLE)) {
				corrected = SecurityConstants.PREFFIX_ROLE + role;
			}
			corrected = corrected.toUpperCase();
			if (SecurityConstants.ROLE_ANONYMOUS.equals(corrected)) {
				return !this.isAuthenticated();
			}
			if (SecurityConstants.ROLE_AUTHENTICATED.equals(corrected)) {
				return this.isAuthenticated();
			}
			return this.session.getRoles().contains(corrected);
		}
		return false;
	}

	public void checkAuthorized(Place fallback, String... roles) {
		if (roles != null) {
			for (String role : roles) {
				if (this.hasRole(role)) {
					return;
				}
			}
		}
		throw new SecurityException("Unauthorized", fallback);
	}

	public void setSession(SessionDto session) {
		if (this.session == null || !this.session.equals(session)) {
			this.session = session;
			this.fireSignIn();
		}
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

	public abstract void signOut();

	public abstract void loadSession();

	protected void fireSignIn() {
		this.fireEvent(new SignInEvent(this.session));
	}

	protected void fireSignFailled() {
		this.fireEvent(new SignFailledEvent(this.session));
	}

	protected void fireSignOut() {
		this.fireEvent(new SignOutEvent(this.session));
		this.fireSignIn();
	}

	public static SessionController get() {
		if (SessionController.instance == null) {
			SessionController.instance = GWT.create(SessionController.class);
		}
		return SessionController.instance;
	}

}
