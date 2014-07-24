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

public class NavLink extends AbstractPanel implements Nav.IsNavContent, CloneableWidget, ClickHandler, ValueChangeHandler<String>, HasDrawable {

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
		endConstruct();
	}

	public NavLink(String label, ScheduledCommand command) {
		this();
		this.command = command;
		setLabel(label);
	}

	protected NavLink(NavLink source) {
		super(source);
		endConstruct();

		setActive(source.active);
		preventClickWhenActive = source.preventClickWhenActive;
		setLink(source.link);
		command = source.command;
		iconType = source.iconType;
		name = source.name;
		setLabel(source.label);
	}

	private void endConstruct() {
		anchor.addDomHandler(this, ClickEvent.getType());
		append(anchor);
	}

	@Override
	public IsWidget cloneWidget() {
		return new NavLink(this);
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
		resetInner();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getIconType() {
		return iconType;
	}

	public void setIconType(String iconType) {
		this.iconType = iconType;
		resetInner();
	}

	public void addAnchorStyle(CssStyle style) {
		StyleUtils.addStyle(anchor, style);
	}

	@Override
	public void setActive(boolean active) {
		this.active = active;
		StyleUtils.toggleStyle(this, LinkStyle.ACTIVE, active);
		if (active && getParent() != null && getParent().getParent() instanceof Nav.IsNavContent) {
			// if link is in dropdown => parent is container and parent.parent is dropdown
			((Nav.IsNavContent) getParent().getParent()).setActive(true);
		}
	}

	@Override
	public boolean isActive() {
		return active;
	}

	public ScheduledCommand getCommand() {
		return command;
	}

	public void setCommand(ScheduledCommand command) {
		this.command = command;
	}

	public void setPreventClickWhenActive(boolean preventClickWhenActive) {
		this.preventClickWhenActive = preventClickWhenActive;
	}

	public String getLink() {
		return this.link;
	}

	public void setLink(String link) {
		this.link = link;
		anchor.setLink(link);
		ensureHistoryChangeHandler();
		redraw();
	}

	private void ensureHistoryChangeHandler() {
		if (historyChangeHandlerRegistration == null) {
			historyChangeHandlerRegistration = History.addValueChangeHandler(this);
		}
	}

	@Override
	public void onClick(ClickEvent event) {
		if (active && preventClickWhenActive) {
			return;
		}
		if (this.command != null) {
			Scheduler.get().scheduleFinally(this.command);
		}
	}

	@Override
	public void onValueChange(ValueChangeEvent<String> event) {
		redraw();
	}

	@Override
	public void redraw() {
		String link = this.link;
		link = link == null ? null : link.replace("#", "");
		if (link != null && link.equals(History.getToken())) {
			setActive(true);
		}
		else {
			setActive(false);
		}
		resetInner();
	}

	private void resetInner() {
		anchor.clear();
		if (label != null) {
			anchor.getElement().setInnerHTML(label);
		}
		if (iconType != null) {
			Icon icon = new Icon();
			icon.setType(iconType);

			anchor.getElement().insertFirst(icon.getElement());
		}

	}

}
