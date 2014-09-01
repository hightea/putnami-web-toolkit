package fr.putnami.pwt.tutorial.client.about;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;

public class AboutView extends Composite implements View<AboutPlace> {

	interface Binder extends UiBinderLocalized<Widget, AboutView> {
		Binder BINDER = GWT.create(Binder.class);
	}

	public AboutView() {
		initWidget(Binder.BINDER.createAndBindUi(this));
	}

	@Override
	public void present(AboutPlace place) {
		// Do Nothing
	}

}
