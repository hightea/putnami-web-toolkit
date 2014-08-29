package fr.putnami.pwt.tutorial.client.welcome;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.plugin.ga.client.GoogleAnalytics;
import fr.putnami.pwt.tutorial.client.about.AboutPlace;

public class WelcomeView extends Composite implements View<WelcomePlace> {

	interface Binder extends UiBinderLocalized<Widget, WelcomeView> {
		Binder BINDER = GWT.create(Binder.class);
	}

	public WelcomeView() {
		initWidget(Binder.BINDER.createAndBindUi(this));
	}

	@Override
	public void present(WelcomePlace place) {
		// Do Nothing
	}

	@UiHandler("aboutButton")
	public void onAboutButton(ButtonEvent event) {
		GoogleAnalytics.get("UA-XXXXXXXX-Y").trackEvent("click", "Welcome About button");
		MvpController.get().goTo(AboutPlace.INSTANCE);
	}

}
