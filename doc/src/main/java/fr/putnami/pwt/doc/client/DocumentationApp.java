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

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.user.client.ui.RootPanel;

import fr.putnami.pwt.core.error.client.ErrorManager;
import fr.putnami.pwt.core.error.client.widget.SimpleErrorDisplayer;
import fr.putnami.pwt.core.inject.client.Module;
import fr.putnami.pwt.core.inject.client.annotation.InjectResource;
import fr.putnami.pwt.core.mvp.client.ActivityFactory;
import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.core.theme.client.CssLink;
import fr.putnami.pwt.core.theme.client.Theme;
import fr.putnami.pwt.core.theme.client.ThemeController;
import fr.putnami.pwt.doc.client.application.DocumentationDisplay;
import fr.putnami.pwt.doc.client.application.error.ApplicationUnreachableExceptionHandler;
import fr.putnami.pwt.doc.client.application.error.ErrorConstants;
import fr.putnami.pwt.doc.client.application.error.UmbrellaExceptionHandler;
import fr.putnami.pwt.doc.client.page.ajaxbot.AjaxBotIndexingPage;
import fr.putnami.pwt.doc.client.page.analytics.GoogleAnalyticsPage;
import fr.putnami.pwt.doc.client.page.binding.DataBindingPage;
import fr.putnami.pwt.doc.client.page.bootstrap.BootstrapPage;
import fr.putnami.pwt.doc.client.page.codeeditor.CodeEditorPage;
import fr.putnami.pwt.doc.client.page.components.ComponentsPage;
import fr.putnami.pwt.doc.client.page.download.DownloadPage;
import fr.putnami.pwt.doc.client.page.errors.ErrorsPage;
import fr.putnami.pwt.doc.client.page.i18n.InternationalizationPage;
import fr.putnami.pwt.doc.client.page.injection.InjectionPage;
import fr.putnami.pwt.doc.client.page.layout.LayoutPage;
import fr.putnami.pwt.doc.client.page.navigation.NavigationPage;
import fr.putnami.pwt.doc.client.page.sample.addressbook.AddressBookPage;
import fr.putnami.pwt.doc.client.page.sample.all.SamplesPage;
import fr.putnami.pwt.doc.client.page.sample.table.ContactsTablePage;
import fr.putnami.pwt.doc.client.page.server.ServerCallsPage;
import fr.putnami.pwt.doc.client.page.soon.ComingSoonPage;
import fr.putnami.pwt.doc.client.page.spring.SpringPage;
import fr.putnami.pwt.doc.client.page.starting.GettingStartedPage;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto1InitGradlePage;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto1InitMavenPage;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto2FirstPagePage;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto3MorePagesPage;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto4BindAFormPage;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto5IssueTrackerPage;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto6ThemePage;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto7GoogleAnalyticsPage;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto8AjaxBotPage;
import fr.putnami.pwt.doc.client.page.welcome.WelcomePage;
import fr.putnami.pwt.plugin.ga.client.GoogleAnalytics;
import static fr.putnami.pwt.doc.client.application.ApplicationConfig.ANALYTICS_TRACKER_ID;
import static fr.putnami.pwt.doc.client.application.ApplicationConfig.DOMAIN;


public class DocumentationApp implements EntryPoint, Module {

	@InjectResource
	DocumentationDisplay display;

	@fr.putnami.pwt.core.inject.client.annotation.EntryPoint
	void onModuleStart() {

	}

	@Override
	public void onModuleLoad() {
		Theme theme = new Theme();
		theme.addLink(new CssLink("theme/doc/style/pwt-doc.css", 0));
		ThemeController.get().installTheme(theme);

		GoogleAnalytics analytics = GoogleAnalytics.init(ANALYTICS_TRACKER_ID, DOMAIN);
		analytics.forceSSL(true);
		analytics.displayfeatures();
		analytics.handleUncaughtException(true);

		RootPanel.get().add(display);

		SimpleErrorDisplayer errorDisplayer = new SimpleErrorDisplayer();
		errorDisplayer.setConstants((ConstantsWithLookup) GWT.create(ErrorConstants.class));
		ErrorManager.get().setErrorDisplayer(errorDisplayer);
		ErrorManager.get().registerErrorHandler(new UmbrellaExceptionHandler());
		ErrorManager.get().registerErrorHandler(new ApplicationUnreachableExceptionHandler());

		final MvpController controller = MvpController.get();
		controller.setDisplay(display);

		controller.setDefaultPlace(new WelcomePage.WelcomePlace());

		controller.registerActivity(GWT.<ActivityFactory> create(WelcomePage.WelcomePlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(GettingStartedPage.GettingStartedPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(BootstrapPage.BootstrapPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(LayoutPage.LayoutsPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(ComponentsPage.ComponentsPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(InjectionPage.InjectionPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(DataBindingPage.DataBindingPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(InternationalizationPage.InternationalizationPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(NavigationPage.NavigationPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(ServerCallsPage.ServerCallsPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(ErrorsPage.ErrorsPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(CodeEditorPage.CodeEditorPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(AjaxBotIndexingPage.AjaxBotIndexingPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(GoogleAnalyticsPage.GoogleAnalyticsPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(SpringPage.SpringPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(SamplesPage.SamplesPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(ContactsTablePage.ContactsTablePlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(AddressBookPage.AddressBookPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(ComingSoonPage.ComingSoonPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(DownloadPage.DownloadPlace.class));

		controller.registerActivity(GWT.<ActivityFactory> create(Tuto1InitGradlePage.Tuto1InitGradlePlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(Tuto1InitMavenPage.Tuto1InitMavenPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(Tuto2FirstPagePage.Tuto2FirstPagePlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(Tuto3MorePagesPage.Tuto3MorePagesPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(Tuto4BindAFormPage.Tuto4BindAFormPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(Tuto5IssueTrackerPage.Tuto5IssueTrackerPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(Tuto6ThemePage.Tuto6ThemePlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(Tuto7GoogleAnalyticsPage.Tuto7GoogleAnalyticsPlace.class));
		controller.registerActivity(GWT.<ActivityFactory> create(Tuto8AjaxBotPage.Tuto8AjaxBotPlace.class));

		controller.handleCurrentHistory();
	}
}
