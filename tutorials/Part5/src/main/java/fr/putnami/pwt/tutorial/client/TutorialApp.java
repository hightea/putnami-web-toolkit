package fr.putnami.pwt.tutorial.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.shared.GWT;
import com.google.gwt.user.client.ui.RootPanel;

import fr.putnami.pwt.core.mvp.client.ActivityFactory;
import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.tutorial.client.about.AboutPlace;
import fr.putnami.pwt.tutorial.client.application.PageDecorator;
import fr.putnami.pwt.tutorial.client.contact.ContactPlace;
import fr.putnami.pwt.tutorial.client.issues.IssuesPlace;
import fr.putnami.pwt.tutorial.client.welcome.WelcomePlace;

public class TutorialApp implements EntryPoint {

	@Override
	public void onModuleLoad() {
		PageDecorator display = GWT.create(PageDecorator.class);
		RootPanel.get().add(display);

		MvpController controller = MvpController.get();
		controller.setDisplay(display);

		controller.setDefaultPlace(new WelcomePlace());
		controller.registerActivity((ActivityFactory) GWT.create(WelcomePlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(AboutPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(ContactPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(IssuesPlace.class));

		// Handles Current token to navigate to the correct place
		controller.handleCurrentHistory();
	}
}
