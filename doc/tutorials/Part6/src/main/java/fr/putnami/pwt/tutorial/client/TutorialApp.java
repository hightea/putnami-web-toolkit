package fr.putnami.pwt.tutorial.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.ui.RootPanel;

import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.core.theme.client.CssLink;
import fr.putnami.pwt.core.theme.client.IconFont;
import fr.putnami.pwt.core.theme.client.Theme;
import fr.putnami.pwt.core.theme.client.ThemeController;
import fr.putnami.pwt.tutorial.client.about.AboutPlace;
import fr.putnami.pwt.tutorial.client.application.PageDecorator;
import fr.putnami.pwt.tutorial.client.contact.ContactPlace;
import fr.putnami.pwt.tutorial.client.issues.IssuesPlace;
import fr.putnami.pwt.tutorial.client.welcome.WelcomePlace;

public class TutorialApp implements EntryPoint {

	@Override
	public void onModuleLoad() {
		PageDecorator display = new PageDecorator();
		RootPanel.get().add(display);

		Theme theme = new Theme();
		theme.addLink(new CssLink("theme/tutorial/style/pwt-tutorial.css", 0));
		IconFont font = new IconFont("theme/tutorial/style/pwt-tutorial-font.css", "icon-");
		// Eventually add aliases :
		font.addAlias("add", "plus");
		font.addAlias("view", "search");

		theme.setIconFont(font);
		ThemeController.get().installTheme(theme);

		MvpController controller = MvpController.get();
		controller.setDisplay(display);

		controller.setDefaultPlace(WelcomePlace.INSTANCE);
		controller.registerActivity(WelcomePlace.INSTANCE);
		controller.registerActivity(AboutPlace.INSTANCE);
		controller.registerActivity(ContactPlace.INSTANCE);
		controller.registerActivity(IssuesPlace.INSTANCE);

		// Handles Current token to navigate to the correct place
		controller.handleCurrentHistory();
	}
}
