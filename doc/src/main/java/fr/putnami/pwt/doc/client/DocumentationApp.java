/**
 * This file is part of pwt-doc.
 *
 * pwt-doc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-doc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-doc.  If not, see <http://www.gnu.org/licenses/>.
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
import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.core.mvp.client.ViewProxy;
import fr.putnami.pwt.core.theme.client.CssLink;
import fr.putnami.pwt.core.theme.client.Theme;
import fr.putnami.pwt.core.theme.client.ThemeController;
import fr.putnami.pwt.doc.client.application.DocumentationDisplay;
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

		MvpController controller = MvpController.get();
		controller.setDisplay(display);

		controller.setDefaultPlace(new WelcomePlace());

		controller.registerActivity((ViewProxy) GWT.create(WelcomePlace.class));
		controller.registerActivity((ViewProxy) GWT.create(GettingStartedPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(BootstrapPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(LayoutsPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(ComponentsPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(DataBindingPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(InternationalizationPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(NavigationPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(ServerCallsPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(ErrorsPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(CodeEditorPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(AjaxBotIndexingPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(GoogleAnalyticsPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(SamplesPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(ContactsTablePlace.class));
		controller.registerActivity((ViewProxy) GWT.create(AddressBookPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(ComingSoonPlace.class));
		controller.registerActivity((ViewProxy) GWT.create(DownloadPlace.class));

		controller.handleCurrentHistory();
	}
}
