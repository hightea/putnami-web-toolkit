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
package fr.putnami.pwt.core.widget.client.boot;

import com.google.gwt.core.client.EntryPoint;

import fr.putnami.pwt.core.theme.client.CssLink;
import fr.putnami.pwt.core.theme.client.ThemeController;

public class WidgetBoot implements EntryPoint {

	@Override
	public void onModuleLoad() {
		ThemeController.get().addDefaultStyle(new CssLink("theme/default/style/bootstrap.min.css", -2));
		ThemeController.get().addDefaultStyle(new CssLink("theme/default/style/fontello.css", -1));
		ThemeController.get().addDefaultStyle(new CssLink("theme/default/style/pwt-core.css", 0));
	}

}
