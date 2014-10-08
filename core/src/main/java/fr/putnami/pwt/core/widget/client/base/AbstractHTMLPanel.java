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

import com.google.common.collect.Iterables;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import java.util.Collections;
import java.util.Map;
import java.util.Set;

import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorComposite;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public abstract class AbstractHTMLPanel extends HTMLPanel
	implements EditorComposite, HasResponsiveVisibility, CloneableWidget {

	protected static final String EMPTY_HTML = "";

	private final Map<Widget, String> children = Maps.newLinkedHashMap();

	private final String html;
	private final String tag;

	private HandlerManager handlerManager;

	private Set<Editor> editorChildren;
	private String path;

	public AbstractHTMLPanel(String tag, String html) {
		super(tag, html);
		this.html = html;
		this.tag = tag;
		StyleUtils.initStyle(this);
	}

	protected AbstractHTMLPanel(AbstractHTMLPanel source) {
		this(source.tag, source.html);
		this.path = source.path;
		for (Map.Entry<Widget, String> widgetEntry : source.children.entrySet()) {
			this.addAndReplaceElement(WidgetUtils.cloneWidget(widgetEntry.getKey()), widgetEntry.getValue());
		}
		this.handlerManager = new HandlerManager(source.handlerManager, this);
		this.handlerManager.resetSinkEvents();
		StyleUtils.cloneStyle(this, source);
	}

	@Override
	protected HandlerManager createHandlerManager() {
		if (this.handlerManager == null) {
			this.handlerManager = new HandlerManager(this);
		}
		return this.handlerManager;
	}

	@Override
	public void sinkEvents(int eventBitsToAdd) {
		super.sinkEvents(eventBitsToAdd);
		this.createHandlerManager().sinkEvents(eventBitsToAdd);
	}

	@Override
	public void unsinkEvents(int eventBitsToRemove) {
		super.sinkEvents(eventBitsToRemove);
		this.createHandlerManager().unsinkEvents(eventBitsToRemove);
	}

	@Override
	public String getPath() {
		return this.path == null ? Path.ROOT_PATH : this.path;
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public Iterable<Editor> getEditors() {
		return this.editorChildren == null ? Collections.<Editor> emptySet() : Iterables
			.unmodifiableIterable(this.editorChildren);
	}

	@Override
	public void addAndReplaceElement(Widget widget, com.google.gwt.user.client.Element toReplace) {
		this.addEditor(widget);
		String elementId = toReplace.getId();
		this.children.put(widget, elementId);
		toReplace.removeAttribute("id");
		super.addAndReplaceElement(widget, toReplace);
	}

	@Override
	@Deprecated
	protected void add(Widget child, com.google.gwt.user.client.Element container) {
		super.add(child, container);
		this.addEditor(child);
	}

	protected void insert(Widget child, int beforeIndex, boolean domInsert) {
		this.insert(child, this.getElement(), beforeIndex, domInsert);
	}

	protected void addEditor(IsWidget widget) {
		if (widget instanceof Editor) {
			if (this.editorChildren == null) {
				this.editorChildren = Sets.newLinkedHashSet();
			}
			this.editorChildren.add((Editor) widget);
		}
	}

	@Override
	public void setStyleName(String style) {
		StyleUtils.addStyle(this, new SimpleStyle(style));
	}

	@Override
	public void setXsVisibility(Visibility xsVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.XS, xsVisibility));
	}

	@Override
	public void setSmVisibility(Visibility smVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.SM, smVisibility));
	}

	@Override
	public void setMdVisibility(Visibility mdVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.MD, mdVisibility));
	}

	@Override
	public void setLgVisibility(Visibility lgVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.LG, lgVisibility));
	}

	@Override
	public void setPrintVisibility(Visibility printVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.PRINT, printVisibility));
	}

}
