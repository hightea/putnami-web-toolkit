package fr.putnami.pwt.tutorial.client;

import fr.putnami.pwt.core.inject.client.Module;
import fr.putnami.pwt.core.inject.client.annotation.MvpDescription;
import fr.putnami.pwt.core.inject.client.annotation.EntryPointHandler;
import fr.putnami.pwt.core.theme.client.CssLink;
import fr.putnami.pwt.core.theme.client.IconFont;
import fr.putnami.pwt.core.theme.client.Theme;
import fr.putnami.pwt.core.theme.client.ThemeController;
import fr.putnami.pwt.tutorial.client.about.AboutPlace;
import fr.putnami.pwt.tutorial.client.application.PageDecorator;
import fr.putnami.pwt.tutorial.client.contact.ContactPlace;
import fr.putnami.pwt.tutorial.client.issues.IssuesPlace;
import fr.putnami.pwt.tutorial.client.welcome.WelcomePlace;

@MvpDescription(
		display = PageDecorator.class,
		defaultPlace = WelcomePlace.class,
		activities = {
				WelcomePlace.class,
				AboutPlace.class,
				ContactPlace.class,
				IssuesPlace.class
		})
public class TutorialApp implements Module {

	@EntryPointHandler
	void installTheme() {
		Theme theme = new Theme();
		theme.addLink(new CssLink("theme/tutorial/style/pwt-tutorial.css", 0));
		IconFont font = new IconFont("theme/tutorial/style/pwt-tutorial-font.css", "icon-");
		// Eventually add aliases :
		font.addAlias("add", "plus");
		font.addAlias("view", "search");

		theme.setIconFont(font);
		ThemeController.get().installTheme(theme);
	}
}

