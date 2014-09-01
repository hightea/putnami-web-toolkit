package fr.putnami.pwt.tutorial.client.contact;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;

public class ContactView extends Composite implements View<ContactPlace> {

	interface Binder extends UiBinderLocalized<Widget, ContactView> {
		Binder BINDER = GWT.create(Binder.class);
	}

	public ContactView() {
		initWidget(Binder.BINDER.createAndBindUi(this));
	}

	@Override
	public void present(ContactPlace place) {
		// Do Nothing
	}

}
