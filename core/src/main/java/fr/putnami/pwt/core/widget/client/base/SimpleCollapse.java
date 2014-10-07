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
package fr.putnami.pwt.core.widget.client.base;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.NavLink;
import fr.putnami.pwt.core.widget.client.helper.CollapseHelper;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class SimpleCollapse extends AbstractDropdown {

	private static final CssStyle STYLE_MENU = new SimpleStyle("list-group");
	private static final CssStyle STYLE_MENU_ITEM = new SimpleStyle("list-group-item");
	private static final CssStyle STYLE_MENU_ITEM_HEADING =
			new SimpleStyle("list-group-item-heading");
	private static final CssStyle STYLE_PANEL = new SimpleStyle("panel");
	private static final CssStyle STYLE_PANEL_DEFAULT = new SimpleStyle("panel-default");
	private static final CssStyle STYLE_PANEL_HEADING = new SimpleStyle("panel-heading");

	private final CollapseHelper collapseHelper;

	public SimpleCollapse(String tagName) {
		super(tagName);
		StyleUtils.addStyle(this.anchor, SimpleCollapse.STYLE_PANEL_HEADING);
		StyleUtils.addStyle(this.menuContainer, SimpleCollapse.STYLE_MENU);
		StyleUtils.addStyle(this, SimpleCollapse.STYLE_PANEL);
		StyleUtils.addStyle(this, SimpleCollapse.STYLE_PANEL_DEFAULT);
		this.collapseHelper = CollapseHelper.apply(this.anchor, this.menuContainer.getElement(), true);
	}

	public SimpleCollapse(String tagName, String label) {
		this(tagName);
		this.setLabel(label);
	}

	@Override
	public void addMenuContent(IsWidget w) {
		if (w instanceof NavLink) {
			((NavLink) w).addAnchorStyle(SimpleCollapse.STYLE_MENU_ITEM_HEADING);
		}
		StyleUtils.addStyle(w.asWidget(), SimpleCollapse.STYLE_MENU_ITEM);
		super.addMenuContent(w);
	}

	@Override
	public void close() {
		this.collapseHelper.collapse();
	}

	@Override
	public void open() {
		this.collapseHelper.expand();
	}

	@Override
	public void toggleOpen() {
		this.collapseHelper.toggleCollapse();
		this.anchor.setFocus(true);
	}

	@Override
	public void onClick(ClickEvent event) {
		// Do Nothing
	}
}
