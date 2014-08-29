package fr.putnami.pwt.tutorial.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.ui.RootPanel;

import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.core.widget.client.OneWidgetPanel;
import fr.putnami.pwt.tutorial.client.welcome.WelcomePlace;

public class TutorialApp implements EntryPoint {

	@Override
	public void onModuleLoad() {
		OneWidgetPanel display = new OneWidgetPanel();
		RootPanel.get().add(display);

		MvpController controller = MvpController.get();
		controller.setDisplay(display);

		controller.setDefaultPlace(WelcomePlace.INSTANCE);
		controller.registerActivity(WelcomePlace.INSTANCE);

		// Handles Current token to navigate to the correct place
		controller.handleCurrentHistory();
	}
}
