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
package fr.putnami.pwt.core.widget.client.helper;

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.user.client.ui.Widget;
import com.google.web.bindery.event.shared.HandlerRegistration;

import fr.putnami.pwt.core.event.client.EventBus;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.event.CollapseEvent;
import fr.putnami.pwt.core.widget.client.event.CollapseEvent.Handler;
import fr.putnami.pwt.core.widget.client.event.CollapseEvent.HasCollapseHandlers;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class CollapseHelper implements ClickHandler, HasCollapseHandlers {

	private static final CssStyle STYLE_COLLAPSING = new SimpleStyle("collapsing");
	private static final CssStyle STYLE_COLLAPSE = new SimpleStyle("collapse");
	private static final CssStyle STYLE_VISIBLE = new SimpleStyle("in");

	private Element collapsableElement;

	private boolean collapsed = false;
	private boolean enabled = true;

	public CollapseHelper(Widget toggleWidget, Element collapsableElement) {
		this.collapsableElement = collapsableElement;
		StyleUtils.addStyle(collapsableElement, CollapseHelper.STYLE_COLLAPSE);
		toggleWidget.addDomHandler(this, ClickEvent.getType());
		this.setInitialCollapse(this.collapsed);
	}

	public void enable() {
		this.enabled = true;
	}

	public boolean isEnabled() {
		return this.enabled;
	}

	public void disable() {
		this.enabled = false;
	}

	public void collapse() {
		this.doCollapse(true);
	}

	public void expand() {
		this.doCollapse(false);
	}

	public void toggleCollapse() {
		this.doCollapse(!this.collapsed);
	}

	@Override
	public void onClick(ClickEvent event) {
		if (this.enabled) {
			this.toggleCollapse();
		}
	}

	private void setInitialCollapse(boolean collapse) {
		this.collapsed = collapse;
		this.collapsableElement.getStyle().clearHeight();
		StyleUtils.removeStyle(this.collapsableElement, CollapseHelper.STYLE_COLLAPSING);
		StyleUtils.toggleStyle(this.collapsableElement, CollapseHelper.STYLE_VISIBLE, !collapse);
	}

	public void doCollapse(final boolean collapse) {
		if (collapse != this.collapsed) {
			EventBus.get().fireEventFromSource(new CollapseEvent(CollapseHelper.this, collapse), CollapseHelper.this);

			this.collapsableElement.getStyle().setHeight(this.collapsableElement.getOffsetHeight(), Unit.PX);

			StyleUtils.removeStyle(this.collapsableElement, CollapseHelper.STYLE_COLLAPSE);
			StyleUtils.removeStyle(this.collapsableElement, CollapseHelper.STYLE_VISIBLE);
			StyleUtils.addStyle(this.collapsableElement, CollapseHelper.STYLE_COLLAPSING);

			final int endHeight = collapse ? 0 : this.collapsableElement.getScrollHeight();

			Scheduler.get().scheduleDeferred(new ScheduledCommand() {

				@Override
				public void execute() {
					CollapseHelper.this.collapsableElement.getStyle().setHeight(endHeight, Unit.PX);
				}
			});

			Scheduler.get().scheduleFixedDelay(new RepeatingCommand() {

				@Override
				public boolean execute() {
					CollapseHelper.this.collapsableElement.getStyle().clearHeight();
					StyleUtils.removeStyle(CollapseHelper.this.collapsableElement, CollapseHelper.STYLE_COLLAPSING);
					StyleUtils.addStyle(CollapseHelper.this.collapsableElement, CollapseHelper.STYLE_COLLAPSE);
					StyleUtils.toggleStyle(CollapseHelper.this.collapsableElement, CollapseHelper.STYLE_VISIBLE, !collapse);
					return false;
				}
			}, 350);
			this.collapsed = collapse;
		}
	}

	@Override
	public HandlerRegistration addCollapseHandler(Handler handler) {
		return EventBus.get().addHandlerToSource(CollapseEvent.TYPE, this, handler);
	}

	@Override
	public void fireEvent(GwtEvent<?> event) {
		EventBus.get().fireEventFromSource(event, this);
	}

	public static CollapseHelper apply(Widget toggleWidget, Element collapsableElement) {
		return new CollapseHelper(toggleWidget, collapsableElement);
	}

	public static CollapseHelper apply(Widget toggleWidget, Element collapsableElement, boolean initialCollapse) {
		CollapseHelper helper = CollapseHelper.apply(toggleWidget, collapsableElement);
		helper.setInitialCollapse(initialCollapse);
		return helper;
	}

}
