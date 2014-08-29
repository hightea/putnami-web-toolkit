package fr.putnami.pwt.tutorial.client.issues;

import com.google.gwt.core.client.GWT;

import fr.putnami.pwt.core.mvp.client.MvpPlace;
import fr.putnami.pwt.core.mvp.client.ViewProxy;

public class IssuesPlace extends MvpPlace {

	public static final IssuesPlace INSTANCE = new IssuesPlace();

	public IssuesPlace() {
		super((ViewProxy) GWT.create(IssuesView.class), null);
	}

	@Override
	public MvpPlace getPlace(String token) {
		return IssuesPlace.INSTANCE;
	}
}
