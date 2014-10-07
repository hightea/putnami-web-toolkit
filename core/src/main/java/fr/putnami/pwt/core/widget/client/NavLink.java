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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.dom.client.LIElement;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.user.client.History;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.web.bindery.event.shared.HandlerRegistration;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.Nav.LinkStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractPanel;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class NavLink extends AbstractPanel
	implements Nav.IsNavContent, CloneableWidget, ClickHandler, ValueChangeHandler<String>,
	HasDrawable {

	private final Anchor<?> anchor = new Anchor();

	private boolean active;
	private boolean preventClickWhenActive = true;

	private ScheduledCommand command;
	private String link;

	private String iconType;
	private String name;
	private String label;

	private HandlerRegistration historyChangeHandlerRegistration;

	public NavLink() {
		super(LIElement.TAG);
		this.endConstruct();
	}

	public NavLink(String label, ScheduledCommand command) {
		this();
		this.command = command;
		this.setLabel(label);
	}

	public NavLink(String label, String link) {
		this();
		this.setLabel(label);
		this.setLink(link);
	}

	protected NavLink(NavLink source) {
		super(source);
		this.endConstruct();

		this.setActive(source.active);
		this.preventClickWhenActive = source.preventClickWhenActive;
		this.setLink(source.link);
		this.command = source.command;
		this.iconType = source.iconType;
		this.name = source.name;
		this.setLabel(source.label);
	}

	private void endConstruct() {
		this.anchor.addDomHandler(this, ClickEvent.getType());
		this.append(this.anchor);
	}

	@Override
	public IsWidget cloneWidget() {
		return new NavLink(this);
	}

	public String getLabel() {
		return this.label;
	}

	public void setLabel(String label) {
		this.label = label;
		this.resetInner();
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getIconType() {
		return this.iconType;
	}

	public void setIconType(String iconType) {
		this.iconType = iconType;
		this.resetInner();
	}

	public void addAnchorStyle(CssStyle style) {
		StyleUtils.addStyle(this.anchor, style);
	}

	@Override
	public void setActive(boolean active) {
		this.active = active;
		this.redraw();
	}

	@Override
	public boolean isActive() {
		return this.active;
	}

	public ScheduledCommand getCommand() {
		return this.command;
	}

	public void setCommand(ScheduledCommand command) {
		this.command = command;
	}

	public void setPreventClickWhenActive(boolean preventClickWhenActive) {
		this.preventClickWhenActive = preventClickWhenActive;
		this.redraw();
	}

	public String getLink() {
		return this.link;
	}

	public void setLink(String link) {
		this.link = link;
		this.anchor.setLink(link);
		this.ensureHistoryChangeHandler();
		this.redraw();
	}

	private void ensureHistoryChangeHandler() {
		if (this.historyChangeHandlerRegistration == null) {
			this.historyChangeHandlerRegistration = History.addValueChangeHandler(this);
		}
	}

	@Override
	public void onClick(ClickEvent event) {
		if (this.active && this.preventClickWhenActive) {
			return;
		}
		if (this.command != null) {
			Scheduler.get().scheduleFinally(this.command);
		}
	}

	@Override
	public void onValueChange(ValueChangeEvent<String> event) {
		this.redraw();
	}

	@Override
	public void redraw() {
		if (this.link != null) {
			String tokenLink = this.link.replaceAll("^#", "");
			if (tokenLink.equals(History.getToken())) {
				this.active = true;
			} else {
				this.active = false;
			}
			if (this.active && this.getParent() != null
				&& this.getParent().getParent() instanceof Nav.IsNavContent) {
				// if link is in dropdown => parent is container and parent.parent is dropdown
				((Nav.IsNavContent) this.getParent().getParent()).setActive(true);
			}
		}
		StyleUtils.toggleStyle(this, LinkStyle.ACTIVE, this.active);
		this.resetInner();
		if (this.active && this.preventClickWhenActive) {
			this.anchor.setLink(null);
		} else if (this.link != null) {
			this.anchor.setLink(this.link);
		} else {
			this.anchor.setLinkAsDummy();
		}
	}

	private void resetInner() {
		this.anchor.clear();
		this.anchor.getElement().removeAllChildren();
		if (this.label != null) {
			this.anchor.getElement().setInnerHTML(this.label);
		}
		if (this.iconType != null) {
			Icon icon = new Icon();
			icon.setType(this.iconType);

			this.anchor.getElement().insertFirst(icon.getElement());
		}
	}

}
