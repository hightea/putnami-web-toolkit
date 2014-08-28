/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client;

import com.google.common.base.Strings;
import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.user.client.Cookies;
import com.google.gwt.user.client.ui.RootPanel;

import fr.putnami.pwt.core.error.client.ErrorManager;
import fr.putnami.pwt.core.error.client.widget.SimpleErrorDisplayer;
import fr.putnami.pwt.core.mvp.client.ActivityFactory;
import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.core.theme.client.CssLink;
import fr.putnami.pwt.core.theme.client.Theme;
import fr.putnami.pwt.core.theme.client.ThemeController;
import fr.putnami.pwt.doc.client.application.DocumentationDisplay;
import fr.putnami.pwt.doc.client.application.error.ApplicationUnreachableExceptionHandler;
import fr.putnami.pwt.doc.client.application.error.ErrorConstants;
import fr.putnami.pwt.doc.client.application.error.UmbrellaExceptionHandler;
import fr.putnami.pwt.doc.client.page.ajaxbot.AjaxBotIndexingPlace;
import fr.putnami.pwt.doc.client.page.analytics.GoogleAnalyticsPlace;
import fr.putnami.pwt.doc.client.page.binding.DataBindingPlace;
import fr.putnami.pwt.doc.client.page.bootstrap.BootstrapPlace;
import fr.putnami.pwt.doc.client.page.codeeditor.CodeEditorPlace;
import fr.putnami.pwt.doc.client.page.components.ComponentsPlace;
import fr.putnami.pwt.doc.client.page.download.DownloadPlace;
import fr.putnami.pwt.doc.client.page.errors.ErrorsPlace;
import fr.putnami.pwt.doc.client.page.i18n.InternationalizationPlace;
import fr.putnami.pwt.doc.client.page.layout.LayoutsPlace;
import fr.putnami.pwt.doc.client.page.navigation.NavigationPlace;
import fr.putnami.pwt.doc.client.page.sample.addressbook.AddressBookPlace;
import fr.putnami.pwt.doc.client.page.sample.all.SamplesPlace;
import fr.putnami.pwt.doc.client.page.sample.table.ContactsTablePlace;
import fr.putnami.pwt.doc.client.page.server.ServerCallsPlace;
import fr.putnami.pwt.doc.client.page.soon.ComingSoonPlace;
import fr.putnami.pwt.doc.client.page.starting.GettingStartedPlace;
import fr.putnami.pwt.doc.client.page.tutorial.TutorialPart1GradlePlace;
import fr.putnami.pwt.doc.client.page.tutorial.TutorialPart1MavenPlace;
import fr.putnami.pwt.doc.client.page.tutorial.TutorialPart2Place;
import fr.putnami.pwt.doc.client.page.tutorial.TutorialPart3Place;
import fr.putnami.pwt.doc.client.page.tutorial.TutorialPart4Place;
import fr.putnami.pwt.doc.client.page.tutorial.TutorialPart5Place;
import fr.putnami.pwt.doc.client.page.tutorial.TutorialPart6Place;
import fr.putnami.pwt.doc.client.page.tutorial.TutorialPart7Place;
import fr.putnami.pwt.doc.client.page.tutorial.TutorialPart8Place;
import fr.putnami.pwt.doc.client.page.welcome.WelcomePlace;
import fr.putnami.pwt.plugin.ga.client.GoogleAnalytics;
import static fr.putnami.pwt.doc.client.application.ApplicationConfig.ANALYTICS_TRACKER_ID;
import static fr.putnami.pwt.doc.client.application.ApplicationConfig.COOKIE_COUNT_VISIT;
import static fr.putnami.pwt.doc.client.application.ApplicationConfig.DOMAIN;

public class DocumentationApp implements EntryPoint {

	@Override
	public void onModuleLoad() {
		Theme theme = new Theme();
		theme.addLink(new CssLink("theme/doc/style/pwt-doc.css", 0));
		ThemeController.get().installTheme(theme);

		if (Cookies.isCookieEnabled()) {
			int cnt = 0;
			try {
				String cntString = Cookies.getCookie(COOKIE_COUNT_VISIT);
				if (!Strings.isNullOrEmpty(cntString)) {
					cnt = Integer.parseInt(cntString);
				}
			}
			catch (NumberFormatException e) {
				cnt = 0;
			}
			cnt++;
			Cookies.setCookie(COOKIE_COUNT_VISIT, "" + cnt);
		}

		GoogleAnalytics analytics = GoogleAnalytics.init(ANALYTICS_TRACKER_ID, DOMAIN);
		analytics.forceSSL(true);
		analytics.displayfeatures();
		analytics.handleUncaughtException(true);

		DocumentationDisplay display = new DocumentationDisplay();
		RootPanel.get().add(display);

		SimpleErrorDisplayer errorDisplayer = new SimpleErrorDisplayer();
		errorDisplayer.setConstants((ConstantsWithLookup) GWT.create(ErrorConstants.class));
		ErrorManager.get().setErrorDisplayer(errorDisplayer);
		ErrorManager.get().registerErrorHandler(new UmbrellaExceptionHandler());
		ErrorManager.get().registerErrorHandler(new ApplicationUnreachableExceptionHandler());

		final MvpController controller = MvpController.get();
		controller.setDisplay(display);

		controller.setDefaultPlace(new WelcomePlace());

		controller.registerActivity((ActivityFactory) GWT.create(WelcomePlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(GettingStartedPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(BootstrapPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(LayoutsPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(ComponentsPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(DataBindingPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(InternationalizationPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(NavigationPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(ServerCallsPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(ErrorsPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(CodeEditorPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(AjaxBotIndexingPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(GoogleAnalyticsPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(SamplesPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(ContactsTablePlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(AddressBookPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(ComingSoonPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(DownloadPlace.class));

		controller.registerActivity((ActivityFactory) GWT.create(TutorialPart1GradlePlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(TutorialPart1MavenPlace.class));
		controller.registerActivity((ActivityFactory) GWT.create(TutorialPart2Place.class));
		controller.registerActivity((ActivityFactory) GWT.create(TutorialPart3Place.class));
		controller.registerActivity((ActivityFactory) GWT.create(TutorialPart4Place.class));
		controller.registerActivity((ActivityFactory) GWT.create(TutorialPart5Place.class));
		controller.registerActivity((ActivityFactory) GWT.create(TutorialPart6Place.class));
		controller.registerActivity((ActivityFactory) GWT.create(TutorialPart7Place.class));
		controller.registerActivity((ActivityFactory) GWT.create(TutorialPart8Place.class));

		controller.handleCurrentHistory();
	}
}
