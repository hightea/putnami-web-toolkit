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
package fr.putnami.pwt.core.widget.client.util;

import com.google.common.base.CaseFormat;
import com.google.gwt.dom.client.Element;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.theme.client.CssStyle;

public final class StyleUtils {

	public static <S extends CssStyle> void removeStyle(Element e, S style) {
		if (e == null) {
			return;
		}
		if (style instanceof Enum) {
			cleanEnumStyle(e, style.getClass());
		}
		String styleName = StyleUtils.getStyle(style);
		String currentClassName = e.getClassName();
		if (styleName != null && currentClassName != null && e.hasClassName(styleName)) {
			e.removeClassName(styleName);
		}
	}

	public static <S extends CssStyle> void removeStyle(Widget w, S style) {
		if (w == null) {
			return;
		}
		StyleUtils.removeStyle(w.getElement(), style);
	}

	public static <S extends CssStyle> void addStyle(Widget w, S style) {
		if (w == null) {
			return;
		}
		StyleUtils.addStyle(w.getElement(), style);
	}

	public static <S extends CssStyle> void addStyle(Element e, S style) {
		if (e == null) {
			return;
		}
		if (style instanceof Enum) {
			cleanEnumStyle(e, style.getClass());
		}
		else if (style instanceof Iterable) {
			for (Object toRemove : (Iterable) style) {
				if (toRemove instanceof CssStyle) {
					CssStyle styleToRemove = (CssStyle) toRemove;
					String styleName = styleToRemove.get();
					if (styleName != null && e.hasClassName(styleName)) {
						e.removeClassName(styleName);
					}
				}
			}
		}
		String styleName = StyleUtils.getStyle(style);
		if (styleName != null) {
			e.addClassName(styleName);
		}
	}

	public static <S extends CssStyle> void toggleStyle(Element e, S style, boolean value) {
		if (e == null || style == null) {
			return;
		}
		if (value) {
			addStyle(e, style);
		}
		else {
			removeStyle(e, style);
		}
	}

	public static <S extends CssStyle> void toggleStyle(Widget w, S style, boolean value) {
		if (w == null) {
			return;
		}
		StyleUtils.toggleStyle(w.getElement(), style, value);
	}

	public static void cleanEnumStyle(Element e, Class<?> enumClass) {
		if (enumClass == null) {
			return;
		}
		for (Object enumValue : enumClass.getEnumConstants()) {
			if (enumValue instanceof CssStyle) {
				String currentClassName = e.getClassName();
				String styleName = ((CssStyle) enumValue).get();
				if (styleName != null && currentClassName != null && e.hasClassName(styleName)) {
					e.removeClassName(styleName);
				}
			}
		}

	}

	public static void initStyle(Widget w) {
		if (w != null && w.getElement() != null) {
			String widgetClassName = "p-" + CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_HYPHEN, w.getClass().getSimpleName());
			w.getElement().addClassName(widgetClassName);
		}
	}

	public static void cloneStyle(Widget target, Widget source) {
		target.getElement().setClassName(source.getElement().getClassName());
	}

	private static <S extends CssStyle> String getStyle(S value) {
		if (value == null || value.get() == null) {
			return null;
		}
		return value.get();
	}

	private StyleUtils() {
	}
}
