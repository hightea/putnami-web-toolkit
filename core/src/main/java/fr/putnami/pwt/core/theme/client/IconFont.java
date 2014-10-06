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
package fr.putnami.pwt.core.theme.client;

import com.google.common.base.Strings;
import com.google.common.collect.Maps;

import java.util.Map;

import fr.putnami.pwt.core.widget.client.base.SimpleStyle;

/**
 * The IconFont is an extention of CssLink.
 * <p>
 * <strong>Fontello :</strong>
 * </p>
 *
 * <pre>
 * IconFont fontello = new IconFont("theme/default/style/fontello.css", "icon-");
 * fontello.addAlias("add", "plus");
 * ThemeController.get().setIconFont(fontello);
 * </pre>
 *
 * @since 1.0
 */
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

	/**
	 * Instantiates a new icon font.
	 *
	 * @param href the link of the icon font css
	 * @param preffix of the icons
	 */
	public IconFont(String href, String preffix) {
		super(href, -1);
		this.preffix = preffix == null ? "" : preffix;
	}

	/**
	 * Adds an icon alias. Allowing to map a fake icon to an other one.
	 *
	 * <pre>
	 * // map the icon "add" to the icon "plus"
	 * font.addAlias("add", "plus");
	 * </pre>
	 *
	 * @param alias the alias
	 * @param target the target
	 */
	public void addAlias(String alias, String target) {
		this.aliases.put(alias, target);
	}

	/**
	 * Gets the {@link CssStyle} of the icon.
	 *
	 * @param iconName the icon name
	 * @return the style
	 */
	public CssStyle getStyle(String iconName) {
		if (Strings.isNullOrEmpty(iconName)) {
			return SimpleStyle.EMPTY_STYLE;
		}
		String className = this.transformClassName(iconName);
		if (this.aliases.containsKey(className)) {
			className = this.aliases.get(className);
		}
		return new SimpleStyle(this.preffix + className);
	}

	/**
	 * Transform icon name to the css class name.
	 * <ol>
	 * <li>transform the iconName to lowerCase</li>
	 * <li>replace '_' with '-'</li>
	 * </ol>
	 *
	 * @param iconName the name of the icon
	 * @return the css class name
	 */
	private String transformClassName(String iconName) {
		return iconName.toLowerCase().replace('_', '-');
	}
}
