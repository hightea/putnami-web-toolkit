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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
import com.google.gwt.dom.client.DivElement;
import com.google.gwt.uibinder.client.UiChild;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractPanel;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public class TabPanelContent extends AbstractPanel implements CloneableWidget {

	private static final CssStyle STYLE_TAB_PANE = new SimpleStyle("tab-pane");
	private static final CssStyle STYLE_ACTIVE = new SimpleStyle("active");
	private static final CssStyle STYLE_FADE = new SimpleStyle("fade");
	private static final CssStyle STYLE_IN = new SimpleStyle("in");

	private NavLink tabLink;

	private boolean active = false;

	public TabPanelContent() {
		super(DivElement.TAG);
		StyleUtils.addStyle(this, STYLE_TAB_PANE);
		StyleUtils.addStyle(this, STYLE_FADE);
	}

	protected TabPanelContent(TabPanelContent source) {
		super(source);
		tabLink = WidgetUtils.cloneWidget(source.tabLink);
		cloneSourceWidgets(source);
	}

	@Override
	public IsWidget cloneWidget() {
		return new TabPanelContent(this);
	}

	public void setActive(final boolean active) {
		this.active = active;
		if (tabLink != null) {
			tabLink.setActive(active);
		}
		StyleUtils.toggleStyle(this, STYLE_IN, active);
		if (active) {
			StyleUtils.addStyle(this, STYLE_ACTIVE);
		}
		else {
			Scheduler.get().scheduleFixedDelay(new RepeatingCommand() {

				@Override
				public boolean execute() {
					StyleUtils.removeStyle(TabPanelContent.this, STYLE_ACTIVE);
					return false;
				}
			}, 150);
		}
	}

	public boolean isActive() {
		return active;
	}

	public NavLink getTabLink() {
		return tabLink;
	}

	@UiChild(limit = 1, tagname = "tabLink")
	public void addTabLink(NavLink tabLink) {
		this.tabLink = tabLink;
	}

	@Override
	public void add(IsWidget child) {
		append(child);
	}

}
