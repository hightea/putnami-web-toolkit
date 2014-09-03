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
package fr.putnami.pwt.core.theme.client;

import java.util.Map;

import com.google.common.base.Strings;
import com.google.common.collect.Maps;

import fr.putnami.pwt.core.widget.client.base.SimpleStyle;

public class IconFont extends CssLink {

	public static final String ICON_CALENDAR = "calendar";
	public static final String ICON_UPLOAD = "upload";
	public static final String ICON_DOWNLOAD = "download";
	public static final String ICON_CANCEL = "cancel";
	public static final String ICON_ADD = "plus";
	public static final String ICON_MENU = "menu";
	public static final String ICON_MAIL = "mail";
	public static final String ICON_DRAG = "drag";
	public static final String ICON_SORT = "sort";
	public static final String ICON_SORT_UP = "sort-up";
	public static final String ICON_SORT_DOWN = "sort-down";
	public static final String ICON_FILTER = "filter";
	public static final String ICON_ANGLE_DOUBLE_RIGHT = "angle-double-right";
	public static final String ICON_ANGLE_DOUBLE_DOWN = "angle-double-down";

	private final String preffix;
	private final Map<String, String> aliases = Maps.newHashMap();

	public IconFont(String href, String preffix) {
		super(href, -1);
		this.preffix = preffix == null ? "" : preffix;
	}

	public void addAlias(String alias, String target) {
		aliases.put(alias, target);
	}

	public CssStyle getStyle(String iconName) {
		if (Strings.isNullOrEmpty(iconName)) {
			return SimpleStyle.EMPTY_STYLE;
		}
		String className = transformClassName(iconName);
		if (aliases.containsKey(className)) {
			className = aliases.get(className);
		}
		return new SimpleStyle(this.preffix + className);
	}

	protected String transformClassName(String iStr) {
		return iStr.toLowerCase().replaceAll("_", "-");
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		return super.equals(obj);
	}

}
