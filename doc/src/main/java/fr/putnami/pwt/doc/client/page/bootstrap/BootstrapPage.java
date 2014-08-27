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
package fr.putnami.pwt.doc.client.page.bootstrap;

import com.google.gwt.uibinder.client.UiHandler;

import fr.putnami.pwt.core.inject.client.annotation.PostConstruct;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.theme.client.CssLink;
import fr.putnami.pwt.core.theme.client.Theme;
import fr.putnami.pwt.core.theme.client.ThemeController;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.doc.client.application.Page;

@Templated
public class BootstrapPage extends Page {

	private final Theme defaultTheme = new Theme();
	private final Theme yeti = new Theme();
	private final Theme amelia = new Theme();
	private final Theme doc = new Theme();

	@PostConstruct
	public void postConstructBootstrapView() {
		yeti.addLink(new CssLink("theme/yeti/style/bootstrap-yeti.min.css", 0));
		amelia.addLink(new CssLink("theme/amelia/style/bootstrap-amelia.min.css", 0));
		doc.addLink(new CssLink("theme/doc/style/pwt.css", 0));
	}


	@UiHandler("yetiBtn")
	public void onYetiClick(ButtonEvent event) {
		ThemeController.get().installTheme(yeti);
	}

	@UiHandler("amaliaBtn")
	public void onAmaliaClick(ButtonEvent event) {
		ThemeController.get().installTheme(amelia);
	}

	@UiHandler("bootstrapBtn")
	public void onBootstrapClick(ButtonEvent event) {
		ThemeController.get().installTheme(defaultTheme);
	}

	@UiHandler("clearBtn")
	public void onClearClick(ButtonEvent event) {
		ThemeController.get().installTheme(doc);
	}
}
