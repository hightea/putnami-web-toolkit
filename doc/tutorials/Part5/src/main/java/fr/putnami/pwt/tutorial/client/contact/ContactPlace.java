package fr.putnami.pwt.tutorial.client.contact;

import com.google.gwt.core.client.GWT;

import fr.putnami.pwt.core.mvp.client.MvpPlace;
import fr.putnami.pwt.core.mvp.client.ViewProxy;

public class ContactPlace extends MvpPlace {

	public static final ContactPlace INSTANCE = new ContactPlace();

	public ContactPlace() {
		super((ViewProxy) GWT.create(ContactView.class), null);
	}

	@Override
	public MvpPlace getPlace(String token) {
		return ContactPlace.INSTANCE;
	}
}
