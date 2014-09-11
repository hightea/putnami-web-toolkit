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

import static fr.putnami.pwt.doc.client.application.ApplicationConfig.ANALYTICS_TRACKER_ID;
import static fr.putnami.pwt.doc.client.application.ApplicationConfig.DOMAIN;
import fr.putnami.pwt.core.error.client.widget.SimpleErrorDisplayer;
import fr.putnami.pwt.core.inject.client.Module;
import fr.putnami.pwt.core.inject.client.annotation.EntryPointHandler;
import fr.putnami.pwt.core.inject.client.annotation.ErrorHandler;
import fr.putnami.pwt.core.inject.client.annotation.ErrorManagmentDescription;
import fr.putnami.pwt.core.inject.client.annotation.MvpDescription;
import fr.putnami.pwt.core.inject.client.annotation.ThemeDescription;
import fr.putnami.pwt.doc.client.application.DocumentationDisplay;
import fr.putnami.pwt.doc.client.application.error.ApplicationUnreachableExceptionHandler;
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
import fr.putnami.pwt.doc.client.page.welcome.WelcomePage.WelcomePlace;
import fr.putnami.pwt.plugin.ga.client.GoogleAnalytics;

@MvpDescription(
		display = DocumentationDisplay.class,
		defaultPlace = WelcomePlace.class,
		activities = {
				WelcomePage.WelcomePlace.class,
				GettingStartedPage.GettingStartedPlace.class,
				BootstrapPage.BootstrapPlace.class,
				LayoutPage.LayoutsPlace.class,
				ComponentsPage.ComponentsPlace.class,
				InjectionPage.InjectionPlace.class,
				DataBindingPage.DataBindingPlace.class,
				InternationalizationPage.InternationalizationPlace.class,
				NavigationPage.NavigationPlace.class,
				ServerCallsPage.ServerCallsPlace.class,
				ErrorsPage.ErrorsPlace.class,
				CodeEditorPage.CodeEditorPlace.class,
				AjaxBotIndexingPage.AjaxBotIndexingPlace.class,
				GoogleAnalyticsPage.GoogleAnalyticsPlace.class,
				SpringPage.SpringPlace.class,
				SamplesPage.SamplesPlace.class,
				ContactsTablePage.ContactsTablePlace.class,
				AddressBookPage.AddressBookPlace.class,
				ComingSoonPage.ComingSoonPlace.class,
				DownloadPage.DownloadPlace.class,
				Tuto1InitGradlePage.Tuto1InitGradlePlace.class,
				Tuto1InitMavenPage.Tuto1InitMavenPlace.class,
				Tuto2FirstPagePage.Tuto2FirstPagePlace.class,
				Tuto3MorePagesPage.Tuto3MorePagesPlace.class,
				Tuto4BindAFormPage.Tuto4BindAFormPlace.class,
				Tuto5IssueTrackerPage.Tuto5IssueTrackerPlace.class,
				Tuto6ThemePage.Tuto6ThemePlace.class,
				Tuto7GoogleAnalyticsPage.Tuto7GoogleAnalyticsPlace.class,
				Tuto8AjaxBotPage.Tuto8AjaxBotPlace.class
		})
@ErrorManagmentDescription(
		errorDisplay = SimpleErrorDisplayer.class,
		errorHandlers = {
				UmbrellaExceptionHandler.class, ApplicationUnreachableExceptionHandler.class
		})
@ThemeDescription(
		styleSheets = "theme/doc/style/pwt-doc.css")
public class DocumentationApp implements Module {

	@EntryPointHandler
	void onModuleStart() {
		GoogleAnalytics analytics = GoogleAnalytics.init(ANALYTICS_TRACKER_ID, DOMAIN);
		analytics.forceSSL(true);
		analytics.displayfeatures();
		analytics.handleUncaughtException(true);

	}

	@ErrorHandler
	boolean handleError(Throwable error) {
		return false;
	}
}
