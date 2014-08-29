package fr.putnami.pwt.tutorial.client.welcome;

import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;

import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.tutorial.client.about.AboutPlace;

@Templated
public class WelcomeView extends Composite implements View {

	@UiHandler("aboutButton")
	public void onAboutButton(ButtonEvent event) {
		MvpController.get().goTo(new AboutPlace());
	}

}
