/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client.boot;

import com.google.gwt.core.client.EntryPoint;

import fr.putnami.pwt.core.theme.client.CssLink;
import fr.putnami.pwt.core.theme.client.IconFont;
import fr.putnami.pwt.core.theme.client.Theme;
import fr.putnami.pwt.core.theme.client.ThemeController;

/**
 * The BootstrapThemeBoot init the default theme with bootstrap, fontello fontawesome, and pwt css
 * resources.
 *
 * <pre>
 *Theme defaultTheme = ThemeController.get().getDefaultTheme();
 *defaultTheme.addLink(new CssLink("theme/default/style/bootstrap.min.css", -2));
 *defaultTheme.addLink(new CssLink("theme/default/style/pwt-core.css", 0));
 *IconFont font = new IconFont("theme/default/style/fontello.css", "icon-");
 *defaultTheme.setIconFont(font);
 *ThemeController.get().installDefaultTheme();
 * </pre>
 *
 */
public class BootstrapThemeLoader implements EntryPoint {

	/*
	 * (non-Javadoc)
	 *
	 * @see com.google.gwt.core.client.EntryPoint#onModuleLoad()
	 */
	@Override
	public void onModuleLoad() {
		Theme defaultTheme = ThemeController.get().getDefaultTheme();
		defaultTheme.addLink(new CssLink("theme/default/style/bootstrap.min.css", -2));
		defaultTheme.addLink(new CssLink("theme/default/style/pwt-core.css", 0));

		IconFont font = new IconFont("theme/default/style/fontello.css", "icon-");
		font.addAlias("add", "plus");
		font.addAlias("save", "floppy");
		font.addAlias("view", "search");
		font.addAlias("drag", "menu");

		defaultTheme.setIconFont(font);
		ThemeController.get().installDefaultTheme();
	}

}
