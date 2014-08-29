package fr.putnami.pwt.tutorial.client.issues;

import java.util.Arrays;
import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.event.FlushSuccessEvent;
import fr.putnami.pwt.core.editor.client.event.ResetDisplayEvent;
import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.service.client.ServiceProxy;
import fr.putnami.pwt.core.service.client.annotation.AsyncHandler;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.Modal;
import fr.putnami.pwt.core.widget.client.TableEditor;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.tutorial.shared.constants.IssueConstants;
import fr.putnami.pwt.tutorial.shared.domain.Issue;
import fr.putnami.pwt.tutorial.shared.service.IssueService;

public class IssuesView extends Composite implements View<IssuesPlace> {

	interface Binder extends UiBinderLocalized<Widget, IssuesView> {
		Binder BINDER = GWT.create(Binder.class);
	}

	interface IssueModel extends Model<Issue> {
		Model<Issue> MODEL = GWT.create(IssueModel.class);
	}

	interface IssueRemoteService extends ServiceProxy<IssuesView, IssueService>, IssueService {
		IssueRemoteService SERVICE = GWT.create(IssueRemoteService.class);
	}

	public static class SearchBean {
		public String label;
		public String name;
	}

	interface SearchConstants extends ConstantsWithLookup {
		@DefaultStringValue("Label")
		String label();

		@DefaultStringValue("Issue label...")
		String labelPlaceholder();

		@DefaultStringValue("Name")
		String name();

		@DefaultStringValue("Issue name...")
		String namePlaceholder();
	}

	interface SearchModel extends Model<SearchBean> {
		Model<SearchBean> MODEL = GWT.create(SearchModel.class);
	}

	@UiField
	Form<SearchBean> searchEditor;

	@UiField
	TableEditor<Issue> issuesTable;

	@UiField(provided = true)
	List<String> labelItems = Arrays.asList("Bug", "Enhancement", "Question", "Duplicate", "Invalid", "WontFix");

	@UiField
	Form<Issue> issueEditor;

	@UiField
	Modal modal;

	public IssuesView() {
		initWidget(Binder.BINDER.createAndBindUi(this));

		MessageHelper helper = new MessageHelper((ConstantsWithLookup) GWT.create(IssueConstants.class));
		issuesTable.setMessageHelper(helper);
		issuesTable.initialize(IssueModel.MODEL);

		MessageHelper searchHelper = new MessageHelper((ConstantsWithLookup) GWT.create(SearchConstants.class));
		searchEditor.setMessageHelper(searchHelper);
		searchEditor.initialize(SearchModel.MODEL);
		searchEditor.edit(new SearchBean());

		issueEditor.setMessageHelper(helper);
		issueEditor.initialize(IssueModel.MODEL);

		IssueRemoteService.SERVICE.bindService(this);
	}

	@Override
	public void present(IssuesPlace place) {
		IssueRemoteService.SERVICE.listIssues();
	}

	@AsyncHandler
	public void onListIssues(List<Issue> issues) {
		issuesTable.edit(issues);
	}

	@UiHandler("searchEditor")
	public void onSearchSubmit(FlushSuccessEvent event) {
		SearchBean search = event.getValue();
		IssueRemoteService.SERVICE.searchIssues(search.name, search.label);
	}

	@UiHandler("searchEditor")
	public void onSearchReset(ResetDisplayEvent event) {
		IssueRemoteService.SERVICE.listIssues();
	}

	@AsyncHandler
	public void onSearchIssues(List<Issue> issues) {
		issuesTable.edit(issues);
	}

	@UiHandler("addIssue")
	public void onAddIssueButton(ButtonEvent event) {
		issueEditor.edit(new Issue());
		modal.show();
	}

	@UiHandler("viewBoutton")
	public void onViewButton(ButtonEvent event) {
		Issue issue = event.getValue();
		issueEditor.edit(issue);
		modal.show();
	}

	@UiHandler("issueEditor")
	public void onSaveIssueEditor(FlushSuccessEvent event) {
		modal.hide();
		Issue issue = event.getValue();
		IssueRemoteService.SERVICE.saveIssue(issue);
	}

	@AsyncHandler
	public void onSaveIssue(Issue issue) {
		searchEditor.edit(new SearchBean());
		IssueRemoteService.SERVICE.listIssues();
	}
}
