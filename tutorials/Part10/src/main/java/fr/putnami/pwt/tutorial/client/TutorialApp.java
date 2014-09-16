package fr.putnami.pwt.tutorial.client;

import fr.putnami.pwt.core.error.client.AbstractErrorHandler;
import fr.putnami.pwt.core.error.client.ErrorManager;
import fr.putnami.pwt.core.inject.client.Module;
import fr.putnami.pwt.core.inject.client.annotation.MvpDescription;
import fr.putnami.pwt.core.inject.client.annotation.EntryPointHandler;
import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.core.security.shared.exception.SecurityException;
import fr.putnami.pwt.core.theme.client.CssLink;
import fr.putnami.pwt.core.theme.client.IconFont;
import fr.putnami.pwt.core.theme.client.Theme;
import fr.putnami.pwt.core.theme.client.ThemeController;
import fr.putnami.pwt.plugin.ga.client.GoogleAnalytics;
import fr.putnami.pwt.tutorial.client.about.AboutPlace;
import fr.putnami.pwt.tutorial.client.application.PageDecorator;
import fr.putnami.pwt.tutorial.client.contact.ContactPlace;
import fr.putnami.pwt.tutorial.client.issues.IssuesPlace;
import fr.putnami.pwt.tutorial.client.welcome.WelcomePlace;
import fr.putnami.pwt.tutorial.client.user.SigninPlace;
import fr.putnami.pwt.tutorial.client.user.SignoutPlace;

@MvpDescription(
		display = PageDecorator.class,
		defaultPlace = WelcomePlace.class,
		activities = {
				WelcomePlace.class,
				AboutPlace.class,
				ContactPlace.class,
				IssuesPlace.class,
				SigninPlace.class
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

	@EntryPointHandler
	void installGA() {
		GoogleAnalytics analytics = GoogleAnalytics.init("UA-XXXXXXXX-Y", "your-domain.tld");
		analytics.forceSSL(true);
		analytics.displayfeatures(); // If you want to enable the display features
		analytics.handleUncaughtException(true); // If you want to handle uncaught exception
	}
	
	@EntryPointHandler
	void catchSecurityError() {
		ErrorManager.get().registerErrorHandler(new AbstractErrorHandler() {
			@Override
			public boolean handle(Throwable error) {
				if (error instanceof SecurityException) {
					MvpController.get().goTo(new SigninPlace(((SecurityException) error).getFallback()));
					return true;
				}
				return false;
			}
		});
	}
	
	@EntryPointHandler
	void registerActions() {
		MvpController.get().registerActivity(new SignoutPlace());
	}
}


