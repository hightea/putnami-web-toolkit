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
import static fr.putnami.pwt.doc.client.application.ApplicationConfig.COOKIE_COUNT_VISIT;
import static fr.putnami.pwt.doc.client.application.ApplicationConfig.DOMAIN;

import com.google.common.base.Strings;
import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.user.client.Cookies;
import com.google.gwt.user.client.ui.RootPanel;

import fr.putnami.pwt.core.error.client.ErrorManager;
import fr.putnami.pwt.core.error.client.widget.SimpleErrorDisplayer;
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
import fr.putnami.pwt.doc.client.page.tutorial.Tuto1InitGradlePlace;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto1InitMavenPlace;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto2FirstPagePlace;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto3MorePagesPlace;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto4BindAFormPlace;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto5IssueTrackerPlace;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto6ThemePlace;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto7GoogleAnalyticsPlace;
import fr.putnami.pwt.doc.client.page.tutorial.Tuto8AjaxBotPlace;
import fr.putnami.pwt.doc.client.page.welcome.WelcomePlace;
import fr.putnami.pwt.plugin.ga.client.GoogleAnalytics;

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

		MvpController controller = MvpController.get();
		controller.setDisplay(display);

		controller.setDefaultPlace(WelcomePlace.INSTANCE);

		controller.registerActivity(WelcomePlace.INSTANCE);
		controller.registerActivity(GettingStartedPlace.INSTANCE);
		controller.registerActivity(BootstrapPlace.INSTANCE);
		controller.registerActivity(LayoutsPlace.INSTANCE);
		controller.registerActivity(ComponentsPlace.INSTANCE);
		controller.registerActivity(DataBindingPlace.INSTANCE);
		controller.registerActivity(InternationalizationPlace.INSTANCE);
		controller.registerActivity(NavigationPlace.INSTANCE);
		controller.registerActivity(ServerCallsPlace.INSTANCE);
		controller.registerActivity(ErrorsPlace.INSTANCE);
		controller.registerActivity(CodeEditorPlace.INSTANCE);
		controller.registerActivity(AjaxBotIndexingPlace.INSTANCE);
		controller.registerActivity(GoogleAnalyticsPlace.INSTANCE);
		controller.registerActivity(SamplesPlace.INSTANCE);
		controller.registerActivity(ContactsTablePlace.INSTANCE);
		controller.registerActivity(AddressBookPlace.INSTANCE);
		controller.registerActivity(ComingSoonPlace.INSTANCE);
		controller.registerActivity(DownloadPlace.INSTANCE);

		controller.registerActivity(Tuto1InitGradlePlace.INSTANCE);
		controller.registerActivity(Tuto1InitMavenPlace.INSTANCE);
		controller.registerActivity(Tuto2FirstPagePlace.INSTANCE);
		controller.registerActivity(Tuto3MorePagesPlace.INSTANCE);
		controller.registerActivity(Tuto4BindAFormPlace.INSTANCE);
		controller.registerActivity(Tuto5IssueTrackerPlace.INSTANCE);
		controller.registerActivity(Tuto6ThemePlace.INSTANCE);
		controller.registerActivity(Tuto7GoogleAnalyticsPlace.INSTANCE);
		controller.registerActivity(Tuto8AjaxBotPlace.INSTANCE);

		controller.registerAlias("Tables", ComponentsPlace.INSTANCE);
		controller.registerAlias("Forms", ComponentsPlace.INSTANCE);
		controller.registerAlias("More", LayoutsPlace.INSTANCE);

		controller.handleCurrentHistory();
	}
}
