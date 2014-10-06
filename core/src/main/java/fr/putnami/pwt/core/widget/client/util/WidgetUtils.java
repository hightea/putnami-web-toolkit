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
package fr.putnami.pwt.core.widget.client.util;

import com.google.common.collect.Sets;
import com.google.gwt.user.client.ui.HasOneWidget;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import java.util.Iterator;
import java.util.Set;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;

public final class WidgetUtils {

	public static <W extends IsWidget> W cloneWidget(W widget) {
		if (widget == null) {
			return null;
		}
		assert widget instanceof CloneableWidget : widget.getClass()
		+ " does not implement CloneableWidget";
		CloneableWidget cloneableWidget = (CloneableWidget) widget;
		return (W) cloneableWidget.cloneWidget();
	}

	public static Set<Widget> listChildren(Widget w) {
		Set<Widget> children = Sets.newHashSet();
		WidgetUtils.collectChildren(w, children);
		return children;
	}

	private static void collectChildren(Widget w, Set<Widget> children) {
		if (w instanceof HasWidgets) {
			HasWidgets hasWidgets = (HasWidgets) w;
			Iterator<Widget> it = hasWidgets.iterator();
			while (it.hasNext()) {
				Widget widget = it.next();
				if (!children.contains(widget)) {
					children.add(widget);
					WidgetUtils.collectChildren(widget, children);
				}
			}
		} else if (w instanceof HasOneWidget) {
			HasOneWidget hasOneWidget = (HasOneWidget) w;
			Widget widget = hasOneWidget.getWidget();
			if (!children.contains(widget)) {
				children.add(widget);
				WidgetUtils.collectChildren(widget, children);
			}
		}
	}

	private WidgetUtils() {
	}
}
