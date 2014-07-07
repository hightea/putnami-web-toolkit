package fr.putnami.pwt.core.security.client.mvp;

import fr.putnami.pwt.core.mvp.client.MvpPlace;
import fr.putnami.pwt.core.mvp.client.ViewProxy;
import fr.putnami.pwt.core.security.client.controller.SessionController;

public abstract class SecuredPlace extends MvpPlace {

	private SessionController sessionController = SessionController.get();

	public SecuredPlace(MvpPlace place, String token) {
		super(place, token);
	}

	public SecuredPlace(MvpPlace place) {
		super(place);
	}

	public SecuredPlace(ViewProxy viewProxy, String token) {
		super(viewProxy, token);
	}

	@Override
	public ViewProxy getViewProxy() {
		if (sessionController.hasRole(getRole())) {
			return super.getViewProxy();
		}
		else {
			return null;
		}
	}

	public String getRole() {
		return null;
	}
}
