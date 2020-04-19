import pygit2


def pull(repo: pygit2.Repository, remote_name: str = 'origin'):
    """
    Pull
    :param repo: the repository to pull
    :param remote_name: name of the remote
    :return:
    """
    for remote in repo.remotes:
        if remote.name == remote_name:
            remote.fetch()
            remote_master_id = repo.lookup_reference('refs/remotes/origin/master').target
            merge_result, _ = repo.merge_analysis(remote_master_id)
            # Up to date, do nothing
            if merge_result & pygit2.GIT_MERGE_ANALYSIS_UP_TO_DATE:
                return
            # We can just fastforward
            elif merge_result & pygit2.GIT_MERGE_ANALYSIS_FASTFORWARD:
                repo.checkout_tree(repo.get(remote_master_id))
                master_ref = repo.lookup_reference('refs/heads/master')
                master_ref.set_target(remote_master_id)
                repo.head.set_target(remote_master_id)
            elif merge_result & pygit2.GIT_MERGE_ANALYSIS_NORMAL:
                repo.merge(remote_master_id)
                print(repo.index.conflicts)

                assert repo.index.conflicts is None, 'Conflicts, ahhhh!'
                user = repo.default_signature
                tree = repo.index.write_tree()
                repo.create_commit('HEAD', user, user, 'Merge!', tree, [repo.head.target, remote_master_id])
                repo.state_cleanup()
            else:
                raise AssertionError('Unknown merge analysis result')
